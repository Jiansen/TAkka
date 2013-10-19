package sample.takka

import takka.actor.ActorRef
import takka.actor.ActorSystem
import takka.actor.Props
import takka.actor.TypedActor

sealed trait Operation
case class Multiplication(m:Int, n:Int) extends Operation
case class Division(m:Int, n:Int) extends Operation

class Calculator extends TypedActor[Operation] {
  def typedReceive = {
    case Multiplication(m:Int, n:Int) =>
      println(m +" * "+ n +" = "+ (m*n))    
    case Division(m, n) =>
      println(m +" / "+ n +" = "+ (m/n))
  }
}

object SupervisorTest extends App{
  val system = ActorSystem("MySystem")
  val calculator:ActorRef[Operation] = system.actorOf(Props[Operation, Calculator], "calculator")
  val multiplicator = calculator.publishAs[Multiplication]
  
  calculator ! Multiplication(3, 2)
  multiplicator ! Multiplication(3, 3)
//  multiplicator ! Division(6, 2)  
  //Compiler Error: type mismatch; found : sample.takka.Division required: 
//	 sample.takka.Multiplication
}

/*
Terminal Output:
3 * 2 = 6
3 * 3 = 9
*/