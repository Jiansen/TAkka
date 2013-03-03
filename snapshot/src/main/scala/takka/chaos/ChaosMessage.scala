package takka.chaos

sealed trait ChaosMessage


/**
 * When an actor receives a Propagation(ratio:Float) message,
 * each of its child has `ratio` possibility of being stopped by
 * the PoisonPill message, and has (1-`ratio`) possibility of forwarding 
 * the Propagation message to all of its children. 
 */
case class Propagation(ratio:Double) extends ChaosMessage


class ChaosException(message:String) extends Exception(message)