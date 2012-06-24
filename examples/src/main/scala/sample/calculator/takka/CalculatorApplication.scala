/**
 *  Copyright (C) 2009-2012 Typesafe Inc. <http://www.typesafe.com>
 */
package typed.remote.calculator

/*
 * comments like //#<tag> are there for inclusion into docs, please don’t remove
 */

import akka.kernel.Bootable
import takka.actor.{ Props, Actor, ActorSystem }
import com.typesafe.config.ConfigFactory

//#actor
class SimpleCalculatorActor extends Actor[CalculatorMessage] {
  def typedReceive = {
    case Op(Add(n1, n2), sender) ⇒
      println("Calculating %d + %d".format(n1, n2))
      sender ! AddResult(n1, n2, n1 + n2)
    case Op(Subtract(n1, n2), sender) ⇒
      println("Calculating %d - %d".format(n1, n2))
      sender ! SubtractResult(n1, n2, n1 - n2)
    /*
    case Op(Add(n1, n2), senderPath) ⇒
      println("Calculating %d + %d".format(n1, n2))
      val sender = typed_context.system.actorFor[MathResult](senderPath)      
      sender ! AddResult(n1, n2, n1 + n2)
    case Op(Subtract(n1, n2), senderPath) ⇒
      val sender = typed_context.system.actorFor[MathResult](senderPath)      
      sender ! SubtractResult(n1, n2, n1 - n2)
      */
  }
}
//#actor

class CalculatorApplication extends Bootable {
  //#setup
  //val system = ActorSystem("CalculatorApplication", ConfigFactory.load.getConfig("calculator"))
  
  val system = ActorSystem("CalculatorApplication",
               ConfigFactory.parseString(""" 
                 akka{
                   actor {
                     provider = "akka.remote.RemoteActorRefProvider"
                   }

                   remote {
                     netty {                       
                       hostname = "129.215.91.88"
                       port = 2552       
                     }
                   }
                 }""") )
  // hostname = "127.0.0.1"
  // hostname = "129.215.91.88"  
  
  // val system = ActorSystem("CalculatorApplication")
  val cal = system.actorOf(Props[CalculatorMessage, SimpleCalculatorActor], "simpleCalculator")
  println(cal.path.address.port)
  //#setup

  def startup() {
  }

  def shutdown() {
    system.shutdown()
  }
}

object CalcApp {
  def main(args: Array[String]) {
    new CalculatorApplication
    println("Started Calculator Application - waiting for messages")
  }
}
