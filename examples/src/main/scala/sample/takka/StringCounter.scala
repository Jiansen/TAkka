package sample.takka

import takka.actor.{TypedActor, ActorRef, ActorSystem, Props}

class StringCounter extends TypedActor[String] {
  var counter = 0;
  def typedReceive = {
    case m => 
      counter = counter +1
      println("received "+counter+" message(s):\n\t"+m)
  }
}

object StringCounterTest extends App {
  val system = ActorSystem("StringCounterTest")
  val counter = system.actorOf(Props[String, StringCounter], "counter")
  
  counter ! "Hello World"
//  counter ! 1
// type mismatch; found : Int(1) required: 
// String  
  val counterString = system.actorFor[String]("akka://StringCounterTest/user/counter")
  counterString ! "Hello World Again"
  val counterInt = system.actorFor[Int]("akka://StringCounterTest/user/counter")  
  counterInt ! 2
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