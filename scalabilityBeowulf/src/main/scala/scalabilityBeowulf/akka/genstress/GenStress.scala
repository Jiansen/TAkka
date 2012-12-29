package scalabilityBeowulf.akka.genstress

/*
This is a generic server benchmark that spawns an echo server 
and a number of clients. Each client fills its message queue 
with a number of dummy messages; it then sends some messages 
to the echo server and waits for its response. 

In The Erlang implementation, the benchmark can be executed 
with or without using the gen_server behaviour.  Because
gen_server is not required in Akka and TAkka, we port the version
without using the gen_server behaviour. 

The benchmark is parametrised by the number of clients, dummy messages 
and messages exchanged with the echo server.
 */
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.{BenchTimer, BenchCounter}
import akka.remote._
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

sealed trait TestorMsg
 /*
    Arguments: np ( the number of clients ),
               n ( the number of messages to the server ), 
               cqueue ( the number of dummy messages ) 
   */
case class GenStressTestMsg(np:Int, n:Int, cqueue:Int) extends ServerMessage
case class ClientEcho(client:ActorRef) extends ServerMessage

case class ClientMsg(server:ActorRef, n:Int) extends ClientMessage// from testor
case class Echo(server:ActorRef, msg:String) extends ServerMessage with ClientMessage//from server

case class ServerMsg(client:ActorRef, msg:String) extends ServerMessage//from client

sealed trait ServerMessage
sealed trait ClientMessage

class GenStressClientActor extends Actor {
  var n:Int = 0
  var server:ActorRef = _
  def receive = {
    case ClientMsg(server, n) =>
      this.n = n
      this.server = server

      for (i<-1 to n) {
        server ! ServerMsg(self, "dummy message")
      }
    case Echo(server, msg) =>
      this.n -= 1
      if (n == 0){
        server ! ClientEcho(self)
      }
  }
}

class GenStressServerActor extends Actor {
  var np:Int = 0
  val timer = new BenchTimer
  var clients:List[ActorRef] = _
  def receive = {
    case GenStressTestMsg(np, n, cqueue) =>
      this.np = np
     
      this.clients = (for (i<- 1 to np) yield {
        context.actorOf(Props[GenStressClientActor], ProcessNamePrefix+i)        
      }).toList
    
      timer.start
      
      for (client <- clients) {
        client ! ClientMsg(self, n)
      }
    case ServerMsg(client, msg) =>
      client ! Echo(self, msg)      
    case ClientEcho(_) =>
      np = np-1
      if (np == 0) {
        timer.finish
        timer.report
        
        context.stop(self)
        for (client <- clients) {
          context.stop(client)
        }
        sys.exit
      }
    
  }
}


object GenBench extends App{
  private val nodes:Int = args(0).toInt
  private val processes = 64
  
  private val system = ActorSystem("GenStressSystem", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, processes, nodes))  
  val master = system.actorOf(Props[GenStressServerActor], ProcessPathPrefix)
  master ! GenStressTestMsg(processes, GenNodeConfig.queue, GenNodeConfig.cqueue)
}
object GenNodeConfig {    
  val queue:Int = 300
  val cqueue:Int = 5000
}
