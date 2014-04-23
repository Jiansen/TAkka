package sample.takka.SupervisedCalculator

import scala.concurrent.duration._
import scala.language.postfixOps
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy._
import akka.actor.SupervisorStrategy
import akka.actor.SupervisorStrategyConfigurator
import takka.actor.ActorRef
import takka.actor.ActorSystem
import takka.actor.Props
import takka.actor.TypedActor
import com.typesafe.config.ConfigFactory

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


class SupervisedCalculator extends TypedActor[Operation] {
  override val supervisorStrategy =
//    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
    OneForOneStrategy() {  
      case _: ArithmeticException  =>
        println("ArithmeticException Raised to: "+typedSelf)
        Restart
    }
  val child:ActorRef[Operation] = typedContext.actorOf(Props[Operation, Calculator], "child")

  def typedReceive = { case m => child ! m  }  
}

object SupervisorTest extends App{
//  val system = ActorSystem("MySystem")
  val system = ActorSystem("MySystem")
  val calculator:ActorRef[Operation] = system.actorOf(Props[Operation, SupervisedCalculator], "calculator")
  val multiple = calculator.publishAs[Multiplication]  // explicit type conversion 
  val divide:ActorRef[Division] = calculator  // contravariance
  
  //  calculator ! "Hello" 
  //    compile error:  type mismatch; found : String("Hello") required: 
  //    sample.takka.SupervisedCalculator.Operation
  calculator ! Multiplication(3, 1)
  calculator ! Division(10, 0)
  calculator ! Division(10, 5)
  calculator ! Division(10, 0)
  calculator ! Multiplication(3, 2)
  calculator ! Division(10, 0)  
  calculator ! Multiplication(3, 3)
  
  Thread.sleep(1000)
  System.out.println("Print")  
  val calOp = system.actorFor[Operation]("akka://MySystem/user/calculator")
  calOp ! Multiplication(3, 4)
  Thread.sleep(1000)
  val calStr = system.actorFor[String]("akka://MySystem/user/calculator")
  calStr ! "Hello" 
  System.out.println("Not Print")
}

/* Terminal Output:
3 * 1 = 3
java.lang.ArithmeticException: / by zero
ArithmeticException Raised to: Actor[akka://MySystem/user/calculator]
10 / 5 = 2
java.lang.ArithmeticException: / by zero
ArithmeticException Raised to: Actor[akka://MySystem/user/calculator]
3 * 2 = 6
java.lang.ArithmeticException: / by zero
ArithmeticException Raised to: Actor[akka://MySystem/user/calculator]
3 * 3 = 9
java.lang.ArithmeticException: / by zero
ArithmeticException Raised to: Actor[akka://MySystem/user/calculator]
Print
3 * 4 = 12
Exception in thread "main" java.lang.Exception: ActorRef[akka://MySystem/user/calculator] does not exist or does not have type ActorRef[String]
*/