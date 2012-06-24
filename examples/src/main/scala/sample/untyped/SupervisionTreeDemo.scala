package sample.untyped

import akka.actor._


class AkkaRootActor extends Actor {
  import context._
  val mid1 = context.actorOf(Props[AkkaMidActor1], "mid1")
  val mid2 = context.actorOf(Props[AkkaMidActor2], "mid2")
  
  def receive = {
    case _  =>
      println(self)
      mid1 ! "print"
      mid2 ! "print"
  }
}

class AkkaMidActor1 extends Actor {
  import context._
  val leaf = context.actorOf(Props[AkkaLeafActor1], "leaf")
  
  def receive = {
    case _  =>
      println(self)
      leaf ! "print"
  }
}

class AkkaMidActor2 extends Actor {
  import context._
  val leaf2 = context.actorOf(Props[AkkaLeafActor2], "leaf2")
  val leaf3 = context.actorOf(Props[AkkaLeafActor3], "leaf3")  
  def receive = {
    case _  =>
      println(self)
      leaf2 ! "print"
      leaf3 ! "print"      
  }
}

class AkkaLeafActor1 extends Actor {
  def receive = {
    case _ => println(self)
  }  
}

class AkkaLeafActor2 extends Actor {
  def receive = {
    case _ => println(self)
  }  
}
class AkkaLeafActor3 extends Actor {
  def receive = {
    case _ => println(self)
  }  
}

object SupervisionTreeDemo extends App{
  println("Hello World")
  val system:ActorSystem = ActorSystem("DemoSystem")
  val root =system.actorOf(Props[AkkaRootActor], "root")
  root ! "print"
  val root2 =system.actorOf(Props[AkkaRootActor], "root2")
  root2 ! "print"
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