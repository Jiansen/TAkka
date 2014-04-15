package sample.akka

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

class StringProcessor extends Actor {
  def receive = {
    case m:String =>
      println("received message: "+m);
  }
}

object StringProcessorTest extends App {
  val system = ActorSystem("StringProcessorTest")
  val processor = system.actorOf(Props[StringProcessor], "processor")
  
  val handler = system.actorOf(Props(new MessageHandler()))
  system.eventStream.subscribe(handler,classOf[akka.actor.UnhandledMessage]);
  processor ! "Hello World"
  processor ! 1
  Thread.sleep(1000)
  val counterRef = system.actorFor("akka://StringProcessorTest/user/processor")
  counterRef ! "Hello World Again"
  counterRef ! 2
}
/*
> sbt run

Enter number: [StringProcessorTest]

Output:
[info] Running StringCounterTest 
received 1 message(s):
	Hello World
received 2 message(s):
	Hello World Again
unhandled message:1
unhandled message:2
 */	