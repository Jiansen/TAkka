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
      child1 ! "print"
      child2 ! "print"
  }
//  this.setChaosLogStream(System.out)
//  this.enableChaos(ChaosChildrenRepeat(0.1, 1 second))
}

class ChildActor extends TypedActor[String] {
  def typedReceive = {
    case _  =>
      println(self)
  }
//  this.setChaosLogStream(System.out)  
}

object SupervisionStopDemo extends App{
  val system:ActorSystem = ActorSystem("DemoSystem")
  val root =system.actorOf(Props[String, SuperActor], "root")
  
  root ! "print"
  Thread.sleep(2000)
  root ! "print"  
}

/*
Actor[akka://DemoSystem/user/root]
Actor[akka://DemoSystem/user/root/child1]
Actor[akka://DemoSystem/user/root/child2]
ChaosChildren Received by: ActorRef[TypeTag[String]]: akka://DemoSystem/user/root
ChaosChildren Received by: ActorRef[TypeTag[String]]: akka://DemoSystem/user/root/child1
ChaosChildren Received by: ActorRef[TypeTag[String]]: akka://DemoSystem/user/root/child2
Actor[akka://DemoSystem/user/root]
Actor[akka://DemoSystem/user/root/child1]
Actor[akka://DemoSystem/user/root/child2]
ChaosChildren Received by: ActorRef[TypeTag[String]]: akka://DemoSystem/user/root
ChaosChildren Received by: ActorRef[TypeTag[String]]: akka://DemoSystem/user/root/child2
ChaosChildren	 Received by: ActorRef[TypeTag[String]]: akka://DemoSystem/user/root/child1
...
*/