/**
 *  Copyright (C) 2009-2012 Typesafe Inc. <http://www.typesafe.com>
 */
package typed.remote.calculator

/*
 * comments like //#<tag> are there for inclusion into docs, please don’t remove
 */

import akka.kernel.Bootable
import com.typesafe.config.ConfigFactory
import scala.util.Random
import takka.actor._

class CreationApplication extends Bootable {
  //#setup
  //val system = ActorSystem("RemoteCreation", ConfigFactory.load.getConfig("remotecreation"))
  val system = ActorSystem("RemoteCreation", 
                           ConfigFactory.parseString(""" 
                 akka{
                   actor {
                     provider = "akka.remote.RemoteActorRefProvider"
                     deployment {
                       /advancedCalculator {
                         remote = "akka://CalculatorApplication@129.215.91.88:2552"
                       }
                     }                               
                   }

                   remote {
                     netty {
                       hostname = "129.215.91.195"
                       port = 2554               
                     }
                   }
                 }""") )
  val localActor = system.actorOf(Props[MathResult, CreationActor], "creationActor")
  val remoteActor = system.actorOf(Props[CalculatorMessage, AdvancedCalculatorActor], "advancedCalculator")

  def doSomething(op: MathOp) = {
    localActor ! Ask(remoteActor, op)
  }
  //#setup

  def startup() {
  }

  def shutdown() {
    system.shutdown()
  }
}

//#actor
class CreationActor extends TypedActor[MathResult] {
  def typedReceive = {
    case Ask(calculator, op) => {
      //calculator ! Op(op, typedSelf.path)
//      calculator ! Op(op, "akka://RemoteCreation@129.215.91.195:2554/user/creationActor")
      calculator ! Op(op, typedRemoteSelf)      
    }
    case result: MathResult => result match {
      case MultiplicationResult(n1, n2, r) => println("Mul result: %d * %d = %d".format(n1, n2, r))
      case DivisionResult(n1, n2, r)       => println("Div result: %.0f / %d = %.2f".format(n1, n2, r))
    }
  }
}
//#actor

object CreationApp {
  def main(args: Array[String]) {
    val app = new CreationApplication
    println("Started Creation Application")
    while (true) {
      if (Random.nextInt(100) % 2 == 0) app.doSomething(Multiply(Random.nextInt(20), Random.nextInt(20)))
      else app.doSomething(Divide(Random.nextInt(10000), (Random.nextInt(99) + 1)))

      Thread.sleep(200)
    }
  }
}
