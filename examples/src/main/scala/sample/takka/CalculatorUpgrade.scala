package sample.takka

import takka.actor.ActorRef
import takka.actor.ActorSystem
import takka.actor.Props
import takka.actor.TypedActor

trait Operation
trait BasicOperation extends Operation
case class Multiplication(m:Int, n:Int) extends BasicOperation
case class Upgrade[Op >: BasicOperation](advancedCalculator:Op=>Unit) extends BasicOperation 

class CalculatorServer extends TypedActor[BasicOperation] { 
  def typedReceive = {
    case Multiplication(m:Int, n:Int) => 
      println(m +" * "+ n +" = "+ (m*n))
    case Upgrade(advancedCalculator) =>
      println("Upgrading ...")
      typedContext.become(advancedCalculator)    
  }
}

object CalculatorUpgrade extends App {
  val system = ActorSystem("CalculatorSystem") 
  val simpleCal:ActorRef[BasicOperation] = system.actorOf(Props[BasicOperation, CalculatorServer], "calculator")
    
  simpleCal ! Multiplication(5, 1)
  case class Divison(m:Int, n:Int) extends Operation
   
  def advancedCalculator:Operation=>Unit = {
    case Multiplication(m:Int, n:Int) => 
      println(m +" * "+ n +" = "+ (m*n))
    case Divison(m:Int, n:Int) => 
      println(m +" / "+ n +" = "+ (m/n))
    case Upgrade(_) =>
      println("Upgraded.")
  }  
     
   simpleCal ! Upgrade(advancedCalculator)    
   val advancedCal = system.actorFor[Operation]("akka://CalculatorSystem/user/calculator")   
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