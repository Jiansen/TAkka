package sample.akka.SupervisedCalculator

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.TypedActor
import akka.actor.SupervisorStrategy._
import akka.actor.OneForOneStrategy
import scala.concurrent.duration._
import scala.language.postfixOps
import akka.actor.UnhandledMessage
import akka.actor.SupervisorStrategyConfigurator
import akka.actor.SupervisorStrategy
import com.typesafe.config.ConfigFactory
import akka.actor.Terminated

case class Multiplication(m:Int, n:Int)
case class Division(m:Int, n:Int)

class CustomizedGuardianSupervisorStrategyConfigurator extends SupervisorStrategyConfigurator{  
  def create(): SupervisorStrategy = {
    OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
      case _ =>
        println("Hello");
        Thread.sleep(2000)        
        Restart
    }    
  }
}

object CustomizedConfig {
  val customConf = ConfigFactory.parseString("""
      akka { actor {
         guardian-supervisor-strategy = "sample.akka.SupervisedCalculator.CustomizedGuardianSupervisorStrategyConfigurator"
      }}
      """)
}

class Calculator extends Actor {
  def receive = {
    case Multiplication(m:Int, n:Int) =>
      println(m +" * "+ n +" = "+ (m*n))    
    case Division(m, n) =>
      println(m +" / "+ n +" = "+ (m/n))
  }
}


class SupervisedCalculator extends Actor {
  override val supervisorStrategy =
//    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
    OneForOneStrategy() {    
      case _: ArithmeticException  =>
        println("ArithmeticException Raised to: "+self)
        Restart
      case t =>
        println(t)
        Restart
        
    }
  val child:ActorRef = context.actorOf(Props[Calculator], "child")

  def receive = { case m => child ! m  }
}

class MessageHandler extends Actor {
  def receive = {
    case UnhandledMessage(message, sender, recipient) =>
      println("unhandled message: "+message);
  }
}

object SupervisorTest extends App{
  val system = ActorSystem("MySystem", CustomizedConfig.customConf)
  val calculator:ActorRef = system.actorOf(Props[SupervisedCalculator], "calculator")

  val handler = system.actorOf(Props[MessageHandler])
  system.eventStream.subscribe(handler,classOf[UnhandledMessage]);  
  calculator ! "Hello" 
  Thread.sleep(1000)
  
  calculator ! Multiplication(3, 1)
  calculator ! Division(10, 0)
  calculator ! Division(10, 5)
  calculator ! Division(10, 0)
  calculator ! Multiplication(3, 2)
  calculator ! Division(10, 0)  
  calculator ! Multiplication(3, 3)
}



/* Terminal Output:
unhandled message: Hello
3 * 1 = 3
java.lang.ArithmeticException: / by zero
ArithmeticException Raised to: Actor[akka://MySystem/user/calculator]
10 / 5 = 2
java.lang.ArithmeticException: / by zero
ArithmeticException Raised to: Actor[akka://MySystem/user/calculator]
3 * 2 = 6
java.lang.ArithmeticException: / by zero
ArithmeticException Raised to: Actor[akka://MySystem/user/calculator]
*/