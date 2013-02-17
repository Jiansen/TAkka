/**
 *  Copyright (C) 2009-2012 Typesafe Inc. <http://www.typesafe.com>
 */
package typed.remote.calculator

/*
 * comments like //#<tag> are there for inclusion into docs, please don’t remove
 */

import akka.kernel.Bootable
import scala.util.Random
//#imports
import com.typesafe.config.ConfigFactory
import takka.actor.{ ActorRef, Props, TypedActor, ActorSystem }
//#imports

class LookupApplication extends Bootable {
  //#setup
  //val system = ActorSystem("LookupApplication", ConfigFactory.load.getConfig("remotelookup"))
  val system = ActorSystem("LookupApplication", 
                           ConfigFactory.parseString(""" 
                 akka{
                   actor {
                     provider = "akka.remote.RemoteActorRefProvider"
                   }

                   remote {
                     netty {
                       hostname = "129.215.91.195"
                       port = 2553               
                     }
                   }
                 }""") )
                 
  // hostname = "129.215.91.195" // Office ethernet (blue cable)
  val actor = system.actorOf(Props[MathResult, LookupActor], "lookupActor")
  val remoteActor = system.actorFor[CalculatorMessage]("akka://CalculatorApplication@129.215.91.88:2552/user/simpleCalculator")

  def doSomething(op: MathOp) = {
    actor ! Ask(remoteActor, op)
  }
  //#setup

  def startup() {
  }

  def shutdown() {
    system.shutdown()
  }
}

//#actor
class LookupActor extends TypedActor[MathResult] {
  def typedReceive = {
    case Ask(calculator, op) => {
//      calculator ! Op(op, "akka://LookupApplication@129.215.91.195:2553/user/lookupActor")
      calculator ! Op(op, typedRemoteSelf)
    }
    case result: MathResult => result match {
      case AddResult(n1, n2, r)      => println("Add result: %d + %d = %d".format(n1, n2, r))
      case SubtractResult(n1, n2, r) => println("Sub result: %d - %d = %d".format(n1, n2, r))
    }
  }
}
//#actor

object LookupApp {
  def main(args: Array[String]) {
    val app = new LookupApplication
    println("Started Lookup Application")
    while (true) {
      if (Random.nextInt(100) % 2 == 0) app.doSomething(Add(Random.nextInt(100), Random.nextInt(100)))
      else app.doSomething(Subtract(Random.nextInt(100), Random.nextInt(100)))

      Thread.sleep(200)
    }
  }
}
