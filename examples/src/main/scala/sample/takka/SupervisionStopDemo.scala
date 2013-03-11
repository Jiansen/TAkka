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
  
  val child1=system.actorFor[String]("akka://DemoSystem/user/root/child1")
  val child2=system.actorFor[String]("akka://DemoSystem/user/root/child2")  

  val chaos = ChaosMonkey(List(child1, child2))
  chaos.enableDebug
  chaos.start(1 second)
}

/*
the following output shows
a) child1 is terminated by PoisonPill (the 1st message)
b) child2 is recovered from an exception (the 2nd message) then is blocked by a non-terminatable calculation (the 3rd message)
 

sending PoisonPill to ActorRef[TypeTag[String]]: akka://DemoSystem/user/root/child1
raising takka.chaos.DefalutChaosException: Default ChaosMonkey Exception at ActorRef[TypeTag[String]]: akka://DemoSystem/user/root/child2
Error: takka.chaos.DefalutChaosException: Default ChaosMonkey Exception
[ERROR] [03/11/2013 03:36:46.523] [DemoSystem-akka.actor.default-dispatcher-4] [akka://DemoSystem/user/root/child2] Default ChaosMonkey Exception
takka.chaos.DefalutChaosException: Default ChaosMonkey Exception
	at takka.chaos.ChaosMonkey$.apply(ChaosMessage.scala:12)
	at sample.takka.SupervisionStopDemo$delayedInit$body.apply(SupervisionStopDemo.scala:46)
	at scala.Function0$class.apply$mcV$sp(Function0.scala:40)
	at scala.runtime.AbstractFunction0.apply$mcV$sp(AbstractFunction0.scala:12)
	at scala.App$$anonfun$main$1.apply(App.scala:71)
	at scala.App$$anonfun$main$1.apply(App.scala:71)
	at scala.collection.immutable.List.foreach(List.scala:318)
	at scala.collection.generic.TraversableForwarder$class.foreach(TraversableForwarder.scala:32)
	at scala.App$class.main(App.scala:71)
	at sample.takka.SupervisionStopDemo$.main(SupervisionStopDemo.scala:39)
	at sample.takka.SupervisionStopDemo.main(SupervisionStopDemo.scala)

running non-ternimatable calculation at ActorRef[TypeTag[String]]: akka://DemoSystem/user/root/child2
running non-ternimatable calculation at ActorRef[TypeTag[String]]: akka://DemoSystem/user/root/child2
sending Kill to ActorRef[TypeTag[String]]: akka://DemoSystem/user/root/child2
running non-ternimatable calculation at ActorRef[TypeTag[String]]: akka://DemoSystem/user/root/child1
sending PoisonPill to ActorRef[TypeTag[String]]: akka://DemoSystem/user/root/child2
...
*/