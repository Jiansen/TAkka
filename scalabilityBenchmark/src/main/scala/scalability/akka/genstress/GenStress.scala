package scalability.akka.genstress

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
import util.BenchTimer

 /*
    Arguments: np ( the number of clients ),
               n ( the number of messages to the server ), 
               cqueue ( the number of dummy messages ) 
   */
case class GenStressTestMsg(np:Int, n:Int, cqueue:Int)
case class ClientEcho(client:ActorRef)

case class ClientMsg(server:ActorRef, n:Int, testor:ActorRef)// from testor
case class Echo(server:ActorRef, msg:Any)//from server

case class ServerMsg(client:ActorRef, msg:Any)//from client


class GenStressServerActor extends Actor {
  def receive = {
    case ServerMsg(client, msg) =>
      client ! Echo(self, msg)
  }
}

class GenStressClientActor(queue:Int) extends Actor {
  var n:Int = 0
  var testor:ActorRef = _
  def receive = {
    case ClientMsg(server, n, testor) =>
      this.n = n
      this.testor = testor

      for (i<-1 to n) {
        server ! ServerMsg(self, "dummy message")
      }
    case Echo(server, msg) =>
      this.n -= 1
      if (n == 0){
        testor ! ClientEcho(self)
      }
  }
}

class GenStressTestActor extends Actor {
  var np:Int = 0
  val timer = new BenchTimer
  var server:ActorRef = _
  var clients:List[ActorRef] = _
  def receive = {
    case GenStressTestMsg(np, n, cqueue) =>
      this.np = np
     
      this.server = context.actorOf(Props[GenStressServerActor])
      this.clients = (for (i<- 1 to np) yield {
        context.actorOf(Props[GenStressClientActor].withCreator(new GenStressClientActor(cqueue)))        
      }).toList
    
      timer.start
      
      for (client <- clients) {
        client ! ClientMsg(server, n, self)
      }
      
    case ClientEcho(_) =>
      np = np-1
      if (np == 0) {
        timer.finish
        timer.report
        
        context.stop(server)
        for (client <- clients) {
          context.stop(client)
        }
        sys.exit
      }
    
  }
}

object GenStress extends App {
  private val timer = new BenchTimer
  private val system = ActorSystem("GenStressSystem")

  val testActor = system.actorOf(Props[GenStressTestActor], "GenStressTestActor")
  testActor ! GenStressTestMsg(1000, 3000, 5000)
  
}