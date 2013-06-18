package sample.takka

import takka.actor.{TypedActor, ActorRef, ActorSystem, Props}
import akka.event.Logging

class ServerActor extends TypedActor[String] {
  def typedReceive = {
    case m:String => println("received message: "+m)
  }
}

object ServerTest extends App {
  val system = ActorSystem("ServerTest")
  val server = system.actorOf(Props[String, ServerActor], "server")
  
  server ! "Hello World"
//  server ! 3
  
  val serverString = system.actorFor[String]("akka://ServerTest/user/server")
  serverString ! "Hello World"
  val serverInt = system.actorFor[Int]("akka://ServerTest/user/server")
  serverInt ! 3
}
/*
> sbt run

Enter number: [ServerTest]

Output:
received message: Hello World
received message: Hello World
[error] (run-main) java.lang.Exception: ActorRef[akka://ServerTest/user/server] does not exist or does not have type ActorRef[Int]
...
 */	