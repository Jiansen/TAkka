package sample.takka

import takka.actor._
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy._
import akka.actor.OneForOneStrategy
import takka.chaos._

class SuperActor extends TypedActor[String] {
  import language.postfixOps
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
      case e  =>
        println("Error: "+e)
        Restart
    
  }

  val child1 = typedContext.actorOf(Props[String, ChildActor], "child1")
  val child2 = typedContext.actorOf(Props[String, ChildActor], "child2")
  
  def typedReceive = {
    case _  =>
      println(self)
//      child1 ! "print"
//      child2 ! "print"
//      context.children foreach ( c => c ! PoisonPill)
      child1 ! "print"
      child2 ! "print"
  }
  
  this.enableChaos(Propagation(0.5))
}

class ChildActor extends TypedActor[String] {
  def typedReceive = {
    case _  =>
      println(self)
      Thread.sleep(2000)
      println(self)
  }
}

object SupervisionStopDemo extends App{
  println("Hello World")
  val system:ActorSystem = ActorSystem("DemoSystem")
  val root =system.actorOf(Props[String, SuperActor], "root")
  
  root ! "print"
}

/*
Hello World

*/