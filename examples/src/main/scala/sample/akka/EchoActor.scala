package sample.akka

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.Logging

class EchoActor extends Actor {
  def receive = {
    case m => println("received message: "+m)
  }
}

object EchoTest extends App {
  val system = ActorSystem("EchoTest")
  val server = system.actorOf(Props[EchoActor], "server") 
  server ! "Hello!"
  server ! 3
  val serverRef = system.actorFor("akka://EchoTest/user/server")
  serverRef ! "Hello Again!"
}
/*
> sbt run

Enter number: [EchoTest]

Output:
[info] Running EchoTest 
received message: Hello!
received message: Hello Again!
 */	