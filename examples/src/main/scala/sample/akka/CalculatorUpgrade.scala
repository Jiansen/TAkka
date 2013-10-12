package sample.akka
import akka.actor._

case object Upgrade
case object Downgrade

case class Mul(m:Int, n:Int) 
case class Div(m:Int, n:Int)

class CalculatorServer extends Actor { 
  import context._

  def receive = simpleCalculator

  def simpleCalculator:Receive = {
    case Mul(m:Int, n:Int) => 
      println(m +" * "+ n +" = "+ (m*n))
    case Upgrade =>
      println("Upgrade")
      become(advancedCalculator, discardOld=false)
    case op =>
      println("Unrecognised operation: "+op)
  }
  def advancedCalculator:Receive = {
    case Mul(m:Int, n:Int) => 
      println(m +" * "+ n +" = "+ (m*n))
    case Div(m:Int, n:Int) => 
      println(m +" / "+ n +" = "+ (m/n))
    case Downgrade =>
      println("Downgrade")
      unbecome();
    case op =>
      println("Unrecognised operation: "+op)
  }  
}

object CalculatorUpgrade extends App {
    val system = ActorSystem("CalculatorSystem") 
    val calculator:ActorRef = system.actorOf(Props[CalculatorServer], "calculator")
    
   calculator ! Mul(5, 1)
   calculator ! Div(10, 1)
   calculator ! Upgrade
   calculator ! Mul(5, 2)
   calculator ! Div(10, 2)
   calculator ! Downgrade
   calculator ! Mul(5, 3)
   calculator ! Div(10, 3)
}


/*  Terminal output:
5 * 1 = 5
Unrecognised operation: Div(10,1)
Upgrade
5 * 2 = 10
10 / 2 = 5
Downgrade
5 * 3 = 15
Unrecognised operation: Div(10,3)
 */