package sample.takka

import takka.actor.{TypedActor, ActorRef, ActorSystem, Props}
import akka.event.Logging

class MyActor extends TypedActor[String] {
  def typedReceive = {
    case "test" => self ! "test2"; println("received test")
    case m      => println("received unknown message: "+m)
  }
}

object FirstActorTest extends App {
  val system = ActorSystem("MySystem")
  val myActor = system.actorOf(Props[String](new MyActor),"myactor")
  myActor ! "test"
  myActor ! "hello"
  
  import takka.nameserver.{NameServer, TSymbol}
  
  NameServer.set(TSymbol[ActorRef[String]]('myActor), myActor)
  val i = NameServer.get(TSymbol[ActorRef[String]]('myActor))
  println(i)
  
}

/*
> sbt run

Enter number: [FirstActorTest]

Output:
Some(ActorRef[Nothing]: akka://MySystem/user/myactor)
received test
received unknown message: hello
received unknown message: test2

 */