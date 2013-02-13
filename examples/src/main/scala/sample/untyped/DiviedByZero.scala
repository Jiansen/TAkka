package sample.untyped

import akka.actor._
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy._

case class Addition(m:Int, n:Int)
case class Subtraction(m:Int, n:Int)
case class Multiplication(m:Int, n:Int)
case class Division(m:Int, n:Int)

class Calculator extends Actor {
  def receive = {
    case Addition(m, n) => 
      println(m +" + "+ n +" = "+ (m+n))
    case Subtraction(m:Int, n:Int) =>
      println(m +" - "+ n +" = "+ (m-n))
    case Multiplication(m:Int, n:Int) =>
      println(m +" * "+ n +" = "+ (m*n))
    case Division(m:Int, n:Int) =>
      println(m +" / "+ n +" = "+ (m/n))
  }
}

class SafeCalculator extends Actor {
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
      case _: ArithmeticException  =>
        println("ArithmeticException Raised to: "+self)
        Restart
    }

  val child:ActorRef = context.actorOf(Props[Calculator], "child")

  def receive = {
    case m => child ! m
  }
}

object SupervisorTest extends App{
  val system = ActorSystem("MySystem")
  val actorRef:ActorRef = system.actorOf(Props[SafeCalculator], "safecalculator")

  actorRef ! Addition(3, 2)
  actorRef ! Division(10, 0)
  actorRef ! Division(10, 5)
}

/*
Terminal Output:
3 + 2 = 5
ArithmeticException Raised to: Actor[akka://MySystem/user/safecalculator]
10 / 5 = 2
*/