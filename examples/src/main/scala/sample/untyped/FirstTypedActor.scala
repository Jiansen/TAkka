import scala.concurrent.{ Promise, Future, Await }
import scala.concurrent.duration._
import akka.actor.{ ActorContext, TypedActor, TypedProps, TypedActorExtension }
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.Logging

trait MyTypedActor{
  def processString(m:String) 
}

class MyTypedActorImpl(val name:String) extends MyTypedActor{
  def this() = this("default")
  
  def processString(m:String) {
    println("received message: "+m) 
  }
}

object FirstTypedActorTest extends App {
  val system = ActorSystem("MySystem")  
  val myTypedActor:MyTypedActor = TypedActor(system).typedActorOf(TypedProps[MyTypedActorImpl]())
  myTypedActor.processString("Hello World")
}

/*
> sbt run

Enter number: [FirstTypedActorTest]

Output:
[info] Running FirstTypedActorTest 
received message: Hello World
 */	