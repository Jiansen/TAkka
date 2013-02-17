/**
 *  Copyright (C) 2009-2012 Typesafe Inc. <http://www.typesafe.com>
 */
package typed.remote.calculator

import takka.actor.{TypedActor, ActorRef}
import akka.actor.ActorPath

trait MathOp

case class Add(nbr1: Int, nbr2: Int) extends MathOp

case class Subtract(nbr1: Int, nbr2: Int) extends MathOp

case class Multiply(nbr1: Int, nbr2: Int) extends MathOp

case class Divide(nbr1: Double, nbr2: Int) extends MathOp

trait MathResult

case class AddResult(nbr: Int, nbr2: Int, result: Int) extends MathResult

case class SubtractResult(nbr1: Int, nbr2: Int, result: Int) extends MathResult

case class MultiplicationResult(nbr1: Int, nbr2: Int, result: Int) extends MathResult

case class DivisionResult(nbr1: Double, nbr2: Int, result: Double) extends MathResult

trait CalculatorMessage
case class Op(op:MathOp, sender:ActorRef[MathResult]) extends CalculatorMessage
//case class Op(op:MathOp, senderPath:String) extends CalculatorMessage

case class Ask(calculator:ActorRef[CalculatorMessage], op:MathOp) extends MathResult

class AdvancedCalculatorActor extends TypedActor[CalculatorMessage] {
  def typedReceive = {
    /*
    case Op(Multiply(n1, n2), senderPath) =>
      println("Calculating %d * %d".format(n1, n2))
      val sender = typed_context.system.actorFor[MathResult](senderPath)
      sender ! MultiplicationResult(n1, n2, n1 * n2)
    case Op(Divide(n1, n2), senderPath) =>
      println("Calculating %.0f / %d".format(n1, n2))
      val sender = typed_context.system.actorFor[MathResult](senderPath)      
      sender ! DivisionResult(n1, n2, n1 / n2)
      */
    case Op(Multiply(n1, n2), sender) =>
      println("Calculating %d * %d".format(n1, n2))
      sender ! MultiplicationResult(n1, n2, n1 * n2)
    case Op(Divide(n1, n2), sender) =>
      println("Calculating %.0f / %d".format(n1, n2))
      sender ! DivisionResult(n1, n2, n1 / n2)
  }
}