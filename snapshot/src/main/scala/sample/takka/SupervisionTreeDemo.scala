package sample.takka

import takka.actor._


class TypedRootActor extends Actor[String] {
//  println("CONTEXT.context  "+actor_context.context)
  val mid1 = typedContext.actorOf(Props(new TypedMidActor1), "mid1")
  val mid2 = typedContext.actorOf(Props(new TypedMidActor2), "mid2")
//  def mid1 = actor_context.actorOf(new TypedMidActor1())
//  def mid2 = actor_context.actorOf(new TypedMidActor2()) 
//  lazy val mid1 = _mid1
//  lazy val mid2 = _mid2
  
  def typedReceive = {
    case _  =>
      println(self)
      mid1 ! "print"
      mid2 ! "print"
  }
}

class TypedMidActor1 extends Actor[String] {

  val leaf = typedContext.actorOf(Props(new TypedLeafActor1), "leaf")
//  def leaf = actor_context.actorOf(new TypedLeafActor1)  
  def typedReceive = {
    case _  =>
      println(self)
      leaf ! "print"
  }
}

class TypedMidActor2 extends Actor[String] {

  val leaf2 = typedContext.actorOf(Props(new TypedLeafActor2), "leaf2")
  val leaf3 = typedContext.actorOf(Props(new TypedLeafActor3), "leaf3")  
//  def leaf2 = actor_context.actorOf(new TypedLeafActor2)
//  def leaf3 = actor_context.actorOf(new TypedLeafActor3)    
  def typedReceive = {
    case _  =>
      println(self)
      leaf2 ! "print"
      leaf3 ! "print"      
  }
}

class TypedLeafActor1 extends Actor[String] {
  def typedReceive = {
    case _ => println(self)
  }  
}

class TypedLeafActor2 extends Actor[String] {
  def typedReceive = {
    case _ => println(self)
  }  
}
class TypedLeafActor3 extends Actor[String] {
  def typedReceive = {
    case _ => println(self)
  }  
}

object SupervisionTreeDemo extends App{
  println("Hello World")
  val system:ActorSystem = ActorSystem("DemoSystem")
//  val root = system.actorOf(Props(new TypedRootActor), "root")
  val root = system.actorOf(Props[String, TypedRootActor], "root")
  root ! "print"
}





/*
Hello World
Actor[akka://DemoSystem/user/root]
Actor[akka://DemoSystem/user/root/mid1]
Actor[akka://DemoSystem/user/root/mid2]
Actor[akka://DemoSystem/user/root/mid1/leaf]
Actor[akka://DemoSystem/user/root/mid2/leaf1]
Actor[akka://DemoSystem/user/root/mid2/leaf3]
*/
// order of output may change occationally