package takka.chaos

import scala.concurrent.duration._

sealed trait ChaosMessage
/**
 * When an actor receives a Propagation(ratio:Float) message,
 * it sends itself a PoisonPill message at `ratio` possibility, or forwards
 * the Propagation message to all of its children. 
 */
case class Propagation(ratio:Double) extends ChaosMessage
/**
 * send itself a PropagationRepeat(ratio:Double) message every `period` time.
 */
case class PropagationRepeat(ratio:Double, period:FiniteDuration) extends ChaosMessage

/**
 * When an actor receives a ChaosChildren(ratio:Double)  message,
 * each of its child has `ratio` possibility of being stopped by
 * the PoisonPill message, and has (1-`ratio`) possibility of forwarding 
 * the ChaosChildren message to all of its children. 
 */
case class ChaosChildren(ratio:Double) extends ChaosMessage
/**
 * send itself a ChaosChildrenRepeat(ratio:Double) message every `period` time.
 */
case class ChaosChildrenRepeat(ratio:Double, period:FiniteDuration) extends ChaosMessage


class ChaosException(message:String) extends Exception(message)