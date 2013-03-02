package sample.untyped

import akka.actor._
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy._

class SuperActor extends Actor {
    
  
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
      case e  =>
        println("Error: "+e)
        Restart
    
  }
  import context._
  val child1 = context.actorOf(Props[ChildActor], "child1")
  val child2 = context.actorOf(Props[ChildActor], "child2")
  
  def receive = {
    case _  =>
      println(self)
//      child1 ! "print"
//      child2 ! "print"
      context.children foreach ( c => c ! PoisonPill)
      child1 ! "print"
      child2 ! "print"
  }
}

class ChildActor extends Actor {
  import context._  
  def receive = {
    case _  =>
      println(self)
      Thread.sleep(2000)
      println(self)
  }
}

object SupervisionStopDemo extends App{
  println("Hello World")
  val system:ActorSystem = ActorSystem("DemoSystem")
  val root =system.actorOf(Props[SuperActor], "root")
  root ! "print"
}

/*
Hello World
Actor[akka://DemoSystem/user/root]
Actor[akka://DemoSystem/user/root/mid1]
Actor[akka://DemoSystem/user/root/mid1/leaf]
Actor[akka://DemoSystem/user/root/mid2]
Actor[akka://DemoSystem/user/root/mid2/leaf2]
Actor[akka://DemoSystem/user/root/mid2/leaf3]
*/