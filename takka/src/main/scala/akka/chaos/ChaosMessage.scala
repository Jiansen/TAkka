package akka.chaos

/*
 * Chaos Monkey Test for Akka Actor.
 * 
 * Random, KILL and PoisonKill mode only.
 * 
 * The TAkka Chaos Monkey has 5 modes: Random, PoisonKill, Kill, Exception, NonTerminate
 * 
 */

import scala.concurrent.duration._
import akka.actor.ActorRef

private [akka] sealed trait ChaosMessage
//private [akka] case class ChaosException(e:Exception) extends ChaosMessage
//private [akka] case object ChaosNonTerminate extends ChaosMessage

object ChaosMonkey {
  def apply(victims:List[ActorRef]):ChaosMonkey ={
    new ChaosMonkey(victims)
  }
}


class ChaosMonkey(victims:List[ActorRef]){
  import scala.concurrent.ExecutionContext.Implicits.global
  import ChaosMode._
  import Status._
  private var mode:ChaosMode = Kill
  private var status:Status = OFF
  private var debugMode:Boolean = false
  
  def setMode(mode:ChaosMode) = {this.mode = mode}
  def enableDebug() = {this.debugMode = true}
  def disableDebug() = {this.debugMode = false}
  
  def start(interval:FiniteDuration) = status match {
    case ON => 
      throw new ChaosMonkeyException("ChaosMonkey is running: turn it off before restart it.") 
    case OFF =>
      setOn
      scala.concurrent.future {
        repeat(interval)
      }
  }
  
  def turnOff() = status match {
    case ON =>
      setOff
    case OFF =>
  }
  
  private def setOn = synchronized{
    status = ON
  }
  
  private def setOff = synchronized{
    status = OFF
  }
  
  private def once() {
    var tempMode = mode
    if (tempMode == Random){
      tempMode = scala.util.Random.shuffle(ChaosMode.values.-(Random).toList).head
    }
    val victim = scala.util.Random.shuffle(victims).head
    tempMode match {
      case PoisonKill =>
        if(this.debugMode) {println("sending PoisonPill to "+victim)}
        victim ! akka.actor.PoisonPill
      case Kill =>
        if(this.debugMode) {println("sending Kill to "+victim)}        
        victim ! akka.actor.Kill
//      case Exception =>
//        val e = scala.util.Random.shuffle(exceptions).head
//        if(this.debugMode) {println("raising "+e+" at "+victim)}        
//        victim.untypedRef ! ChaosException(e)
//      case NonTerminate =>
//        if(this.debugMode) {println("running non-ternimatable calculation at "+victim)}        
//        victim.untypedRef ! ChaosNonTerminate
    }
  }
  
  private def repeat(period:FiniteDuration):Unit =  status match {
    case ON =>
      once
      Thread.sleep(period.toMillis)
      repeat(period)      
    case OFF =>
  }
}

/*
 * ChaosMode:
 *   Random: randomly choose one of other mode in each test
 *   PoisonPill: send a PoisonKill message to a random actor
 *   Kill: send a Kill message to a random actor
 *   Exception: raise an Exception in a random actor
 *   InfiniteLoop: compute an non-terminatable calculation in a random actor
 */
@serializable
object ChaosMode extends Enumeration {
    type ChaosMode = Value
//    val Random, PoisonKill, Kill, Exception, NonTerminate  = Value
    val Random, PoisonKill, Kill = Value
}

@serializable
private[chaos] object Status extends Enumeration {
  type Status = Value
  val ON, OFF = Value
}

class ChaosMonkeyException(message:String) extends Exception(message)
//class DefalutChaosException extends Exception("Default ChaosMonkey Exception")