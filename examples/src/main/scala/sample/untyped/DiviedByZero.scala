package sample.untyped

import akka.actor._
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy._

case class Division(m:Int, n:Int)
case class GCD(m:Int, n:Int)

class Calculator extends Actor {
  def receive = {
    case Division(m, n) =>
      println(m +" / "+ n +" = "+ (m/n))
    case GCD(m, n) =>
     println("GCD of "+m+" and "+n+" is "+gcd(m,n)) 
  }
  
  def gcd(m:Int, n:Int):Int = {
    var tempm = m;
    var tempn = n
    while (tempm != tempn){
      println(tempm + "  "+ tempn)
      if (tempm > tempn){
        tempm = tempm-tempn
      }else{
        tempn = tempn-tempm
      }
    }
    tempm
  }
  
}

class SafeCalculator extends Actor {
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
      case _: ArithmeticException  =>
        println("ArithmeticException Raised to: "+self)
        Restart
    }

  val child:ActorRef = context.actorOf(Props[Calculator], "child")

  def receive = {
    case m => child ! m
  }
}

class Super extends Actor{
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
      case e  =>
        println("Exception "+e+"Raised to: "+self)
        Restart
    }

  val child:ActorRef = context.actorOf(Props[SafeCalculator], "safe")

  def receive = {
    case m => child ! m
  }  
}

object SupervisorTest extends App{
  val system = ActorSystem("MySystem")
//  val calculator:ActorRef = system.actorOf(Props[SafeCalculator], "safecalculator")
  val calculator:ActorRef = system.actorOf(Props[Super], "super")

  calculator ! Division(10, 0)
  calculator ! Division(10, 5)
  calculator ! Division(10, 0)
  calculator ! Division(10, 5)
  calculator ! Division(10, 0)
  calculator ! Division(10, 5)
//  actorRef ! GCD(0, 72)
  calculator ! Division(12, 6)
}

/*
Terminal Output:
ArithmeticException Raised to: Actor[akka://MySystem/user/safecalculator]
10 / 5 = 2
ArithmeticException Raised to: Actor[akka://MySystem/user/safecalculator]
10 / 5 = 2
ArithmeticException Raised to: Actor[akka://MySystem/user/safecalculator]
*/