package scalabilityBeowulf.takka.genstress

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
import takka.actor.{Actor, ActorRef, ActorSystem, Props}
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
case class ClientEcho(client:ActorRef[ClientMsg]) extends ServerMessage

case class ClientMsg(server:ActorRef[ServerMessage], n:Int) extends ClientMessage// from testor
case class Echo(server:ActorRef[ServerMessage], msg:String) extends ServerMessage with ClientMessage//from server

case class ServerMsg(client:ActorRef[ClientMessage], msg:String) extends ServerMessage//from client

sealed trait ServerMessage
sealed trait ClientMessage

class GenStressClientActor extends Actor[ClientMessage] {
  var n:Int = 0
  var server:ActorRef[ServerMessage] = _
  def typedReceive = {
    case ClientMsg(server, n) =>
      this.n = n
      this.server = server

      for (i<-1 to n) {
        server ! ServerMsg(typedSelf, "dummy message")
      }
    case Echo(server, msg) =>
      this.n -= 1
      if (n == 0){
        server ! ClientEcho(typedSelf.publishAs[ClientMsg])
      }
  }
}

class GenStressServerActor extends Actor[ServerMessage] {
  var np:Int = 0
  val timer = new BenchTimer
  var server:ActorRef[ServerMsg] = _
  var clients:List[ActorRef[ClientMessage]] = _
  def typedReceive = {
    case GenStressTestMsg(np, n, cqueue) =>
      this.np = np
     
      this.clients = (for (i<- 1 to np) yield {
        typedContext.actorOf(Props[ClientMessage, GenStressClientActor], GenNodeConfig.ProcessNamePrefix+i)        
      }).toList
    
      timer.start
      
      for (client <- clients) {
        client ! ClientMsg(typedSelf, n)
      }
    case ServerMsg(client, msg) =>
      client ! Echo(typedSelf, msg)      
    case ClientEcho(_) =>
      np = np-1
      if (np == 0) {
        timer.finish
        timer.report
        
        typedContext.stop(server)
        for (client <- clients) {
          typedContext.stop(client)
        }
        sys.exit
      }
    
  }
}


object GenBench extends App{
  private val nodes:Int = args(0).toInt
  private val processes = 10
  
  private val system = ActorSystem("GenStressSystem", masterNodeConfig(GenNodeConfig.WorkerNodePrefix, GenNodeConfig.ProcessPathPrefix, GenNodeConfig.ProcessNamePrefix, processes, nodes))  
  val master = system.actorOf(Props[ServerMessage, GenStressServerActor], "GenStressBenchActor")
  master ! GenStressTestMsg(processes, GenNodeConfig.queue, GenNodeConfig.cqueue)
}

object GenNode extends App {
  private val nodeID:Int = args(0).toInt

  private val system = ActorSystem(GenNodeConfig.WorkerNodePrefix+nodeID, WorkerNodeConfig(nodeID))
}

object GenNodeConfig {
  val WorkerNodePrefix = "GenStressNode"
  val ProcessPathPrefix = "GenStressBenchActor"
  val ProcessNamePrefix = "GenStressProcess"
    
  val queue:Int = 3000
  val cqueue:Int = 5000
}
