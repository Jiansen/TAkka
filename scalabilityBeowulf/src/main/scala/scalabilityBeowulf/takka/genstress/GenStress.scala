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
import takka.actor.{TypedActor, ActorRef, ActorSystem, Props}
import util.{BenchTimer, BenchCounter}
import akka.remote._
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

import takka.chaos._
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy._
import akka.actor.OneForOneStrategy

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

class GenStressClientActor extends TypedActor[ClientMessage] {
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

class GenStressServerActor extends TypedActor[ServerMessage] {
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
      case e  =>
//        println("Error: "+e)
        Resume    
  }
  
  var counter = new BenchCounter
  val timer = new BenchTimer
  var clients:List[ActorRef[ClientMessage]] = _
  def typedReceive = {
    case GenStressTestMsg(np, n, cqueue) =>
      counter.set(np)
     
      this.clients = (for (i<- 1 to np) yield {
        typedContext.actorOf(Props[ClientMessage, GenStressClientActor], ProcessNamePrefix+i)        
      }).toList
    
      
      if(util.Configuration.EnableChaos){
        import takka.chaos.ChaosMode._
        val chaos = ChaosMonkey(clients)
        chaos.setMode(Kill)
//        chaos.enableDebug
        chaos.start(1 second)
      }
      
      timer.start
      
      for (client <- clients) {
        client ! ClientMsg(typedSelf, n)
      }
    case ServerMsg(client, msg) =>
      client ! Echo(typedSelf, msg)      
    case ClientEcho(_) =>
      counter.decrement
      if(util.Configuration.TraceProgress){
            println("Wating another : "+counter.get+" processes to finish.")
      }
      if (counter.isZero) {
        timer.finish
        timer.report
        
        typedContext.stop(typedSelf)
        for (client <- clients) {
          typedContext.stop(client)
        }
        sys.exit
      }
    
  }
}


object GenBench extends App{
  private val nodes:Int = args(0).toInt
  private val processes = 64
  
  private val system = ActorSystem("GenStressSystem", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, processes, nodes))  
  val master = system.actorOf(Props[ServerMessage, GenStressServerActor], ProcessPathPrefix)
  master ! GenStressTestMsg(processes, GenNodeConfig.queue, GenNodeConfig.cqueue)
}

object GenNodeConfig {
  val queue:Int = 300
  val cqueue:Int = 5000
}
