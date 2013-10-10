package sample.akka

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.Logging

class StringCounter extends Actor {
  var counter = 0;
  def receive = {
    case m:String => 
      counter = counter +1
      println("received "+counter+" message(s):\n\t"+m)
  }
}

object StringCounterTest extends App {
  val system = ActorSystem("StringCounterTest")
  val counter = system.actorOf(Props[StringCounter], "counter")
  
  val handler = system.actorOf(Props(new MessageHandler()))
  system.eventStream.subscribe(handler,classOf[akka.actor.UnhandledMessage]);
  counter ! "Hello World"
  counter ! 1
  val counterRef = system.actorFor("akka://StringCounterTest/user/counter")
  counterRef ! "Hello World Again"
  counterRef ! 2
}
/*
> sbt run

Enter number: [StringCounterTest]

Output:
[info] Running StringCounterTest 
received 1 message(s):
	Hello World
received 2 message(s):
	Hello World Again
unhandled message:1
unhandled message:2
 */	