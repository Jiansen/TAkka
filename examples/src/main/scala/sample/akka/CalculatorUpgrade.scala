package sample.akka

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Actor

case class Multiplication(m:Int, n:Int)
case class Upgrade(advancedCalculator:PartialFunction[Any,Unit])

class CalculatorServer extends Actor { 
  def receive = {
    case Multiplication(m:Int, n:Int) => 
      println(m +" * "+ n +" = "+ (m*n))
    case Upgrade(advancedCalculator) =>
      println("Upgrading ...")
      context.become(advancedCalculator)    
  }
}

object CalculatorUpgrade extends App {
  val system = ActorSystem("CalculatorSystem") 
  val simpleCal:ActorRef = system.actorOf(Props[CalculatorServer], "calculator")
    
  simpleCal ! Multiplication(5, 1)
  case class Divison(m:Int, n:Int) 
   
  def advancedCalculator:PartialFunction[Any,Unit] = {
    case Multiplication(m:Int, n:Int) => 
      println(m +" * "+ n +" = "+ (m*n))
    case Divison(m:Int, n:Int) => 
      println(m +" / "+ n +" = "+ (m/n))
    case Upgrade(_) =>
      println("Upgraded.")
  }  
     
   simpleCal ! Upgrade(advancedCalculator)    
   val advancedCal = system.actorFor("akka://CalculatorSystem/user/calculator")   
   advancedCal ! Multiplication(5, 3)
   advancedCal ! Divison(10, 3)
   advancedCal ! Upgrade(advancedCalculator)
}


/*  Terminal Output:
5 * 1 = 5
Upgrading ...
5 * 3 = 15
10 / 3 = 3
Upgraded.
 */