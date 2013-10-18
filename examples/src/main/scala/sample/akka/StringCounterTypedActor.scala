package sample.akka;

import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.TypedActor
import akka.actor.TypedProps
import akka.actor.UnhandledMessage

trait StringCounterTypedActor{
  def processString(m:String) 
}

class StringCounterTypedActorImpl (val name:String) extends StringCounterTypedActor{
  private var counter = 0;
  def this() = this("default")
  
  def processString(m:String) {
    counter = counter +1
    println("received "+counter+" message(s):\n\t"+m)
  }
}

object StringCounterTypedActorTest extends App {
  val system = ActorSystem("StringCounterTest")
  val counter:StringCounterTypedActor = TypedActor(system).typedActorOf(TypedProps[StringCounterTypedActorImpl](), "counter")
  counter.processString("Hello World")
  
  val handler = system.actorOf(Props(new MessageHandler()))
  system.eventStream.subscribe(handler,classOf[akka.actor.UnhandledMessage]);
  
//  counter ! "Hello World"  
//  Compiler Error:  
//   value ! is not a member of 
//	   sample.akka.StringCounterTypedActor
  val counterRef = system.actorFor("akka://StringCounterTest/user/counter")
  counterRef ! "Hello World Again"
  counterRef ! 2
}
/*
> sbt run

Enter number: [StringCounterTypedActorTest]

Output:
[info] Running StringCounterTest 
received 1 message(s):
	Hello World
unhandled message:Hello World Again
unhandled message:2
 */	