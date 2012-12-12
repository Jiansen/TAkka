package scalability.takka.genstress

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
import util.BenchTimer

sealed trait TestorMsg
 /*
    Arguments: np ( the number of clients ),
               n ( the number of messages to the server ), 
               cqueue ( the number of dummy messages ) 
   */
case class GenStressTestMsg(np:Int, n:Int, cqueue:Int) extends TestorMsg
case class ClientEcho(client:ActorRef[ClientMsg]) extends TestorMsg

case class ClientMsg(server:ActorRef[ServerMsg], n:Int, testor:ActorRef[TestorMsg]) extends ClientMessage// from testor
case class Echo(server:ActorRef[ServerMsg], msg:String) extends ServerMessage with ClientMessage//from server

case class ServerMsg(client:ActorRef[ClientMessage], msg:String) extends ServerMessage//from client

sealed trait ServerMessage
sealed trait ClientMessage

class GenStressServerActor extends Actor[ServerMsg] {
  def typedReceive = {
    case ServerMsg(client, msg) =>
      client ! Echo(typedSelf, msg)
  }
}

class GenStressClientActor(queue:Int) extends Actor[ClientMessage] {
  var n:Int = 0
  var testor:ActorRef[TestorMsg] = _
  def typedReceive = {
    case ClientMsg(server, n, testor) =>
      this.n = n
      this.testor = testor

      for (i<-1 to n) {
        server ! ServerMsg(typedSelf, "dummy message")
      }
    case Echo(server, msg) =>
      this.n -= 1
      if (n == 0){
        testor ! ClientEcho(typedSelf)
      }
  }
}

class GenStressTestActor extends Actor[TestorMsg] {
  var np:Int = 0
  val timer = new BenchTimer
  var server:ActorRef[ServerMsg] = _
  var clients:List[ActorRef[ClientMessage]] = _
  def typedReceive = {
    case GenStressTestMsg(np, n, cqueue) =>
      this.np = np
     
      this.server = typedContext.actorOf(Props[ServerMsg, GenStressServerActor])
      this.clients = (for (i<- 1 to np) yield {
        typedContext.actorOf(Props[ClientMessage].withCreator(new GenStressClientActor(cqueue)))        
      }).toList
    
      timer.start
      
      for (client <- clients) {
        client ! ClientMsg(server, n, typedSelf)
      }
      
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

object GenStress extends App {
  private val system = ActorSystem("GenStressSystem")

  val testActor = system.actorOf(Props[TestorMsg, GenStressTestActor], "GenStressTestActor")
  testActor ! GenStressTestMsg(1000, 3000, 5000)
  
}