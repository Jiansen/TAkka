package sample.takka

import takka.actor.{TypedActor, ActorRef, ActorSystem, Props}

class StringProcessor extends TypedActor[String] {
  def typedReceive = {
      case m => println("received message: "+m)
  }
}

object StringProcessorTest extends App {
  val system = ActorSystem("StringProcessorTest")
  val processor = system.actorOf(Props[String, StringProcessor], "processor")
  
  processor ! "Hello World"
//  processor ! 1
// type mismatch; found : Int(1) required: 
// String  
  
  Thread.sleep(1000)
  val processorString = system.actorFor[String]("akka://StringProcessorTest/user/processor")
  processorString ! "Hello World Again"
  val processorInt = system.actorFor[Int]("akka://StringProcessorTest/user/processor")  
  println("This line should not be excuted");
  processorInt ! 2
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
Exception in thread "main" java.lang.Exception: ActorRef[akka://StringCounterTest/user/counter] does not exist or does not have type ActorRef[Int]
 */	