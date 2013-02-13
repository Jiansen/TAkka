
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.Logging

class MyActor extends Actor {
  def receive = {
    case m:String => println("received message: "+m)
  }
}

class MessageHandler(system: ActorSystem) extends Actor {
  def receive = {
    case akka.actor.UnhandledMessage(message, sender, recipient) =>
      println("unhandled message:"+message);
  }
}

object FirstActorTest extends App {
  val system = ActorSystem("MySystem")
  val myActor = system.actorOf(Props[MyActor], "myactor")
  
  val handler = system.actorOf(Props(new MessageHandler(system)))
  system.eventStream.subscribe(handler,classOf[akka.actor.UnhandledMessage]);
  
  myActor ! "Hello World"
  myActor ! 3
}

/*
> sbt run

Enter number: [FirstActorTest]

Output:
[info] Running FirstActorTest 
received message: Hello World
unhandled message:3
 */	