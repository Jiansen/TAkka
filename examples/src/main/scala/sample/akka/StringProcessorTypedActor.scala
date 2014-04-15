package sample.akka;

import akka.actor.{ActorSystem, Props, TypedActor, TypedProps, UnhandledMessage}

trait StringProcessorTypedActor{
  def processString(m:String) 
}

class StringProcessorTypedActorImpl (val name:String) extends StringProcessorTypedActor{
  def this() = this("default")
  
  def processString(m:String) {
      println("received message: "+m);
  }
}

object StringProcessorTypedActorTest extends App {
  val system = ActorSystem("StringProcessorTest")
  val processor:StringProcessorTypedActor = TypedActor(system).typedActorOf(TypedProps[StringProcessorTypedActorImpl](), "processor")
  processor.processString("Hello World")
  
  val handler = system.actorOf(Props(new MessageHandler()))
  system.eventStream.subscribe(handler,classOf[akka.actor.UnhandledMessage]);
  
//  processor ! "Hello World"  
//  Compiler Error:  
//   value ! is not a member of 
//	   sample.akka.StringCounterTypedActor
  val processorRef = system.actorFor("akka://StringProcessorTest/user/processor")
  processorRef ! "Hello World Again"
  processorRef ! 2
  
  val processorTypedRef:StringProcessorTypedActor = TypedActor(system).typedActorOf(TypedProps[StringProcessorTypedActorImpl](), processorRef)
  processorTypedRef.processString("Hello World Again and Again")
  
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
received message: Hello World Again and Again
 */	