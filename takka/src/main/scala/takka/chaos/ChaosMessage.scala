package takka.chaos

import scala.concurrent.duration._
import takka.actor.ActorRef

private [takka] sealed trait ChaosMessage
private [takka] case class ChaosException(e:Exception) extends ChaosMessage
private [takka] case object ChaosNonTerminate extends ChaosMessage

object ChaosMonkey {
  def apply(victims:List[ActorRef[_]]):ChaosMonkey ={
    new ChaosMonkey(victims, List(new DefalutChaosException))
  }
  def apply(victims:List[ActorRef[_]], exceptions:List[Exception]):ChaosMonkey = {
    new ChaosMonkey(victims, exceptions)
  }
}


class ChaosMonkey(victims:List[ActorRef[_]], exceptions:List[Exception]){
  import scala.concurrent.ExecutionContext.Implicits.global
  import ChaosMode._
  import Status._
  private var mode:ChaosMode = Random
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
        victim.untypedRef ! akka.actor.PoisonPill
      case Kill =>
        if(this.debugMode) {println("sending Kill to "+victim)}        
        victim.untypedRef ! akka.actor.Kill
      case Exception =>
        val e = scala.util.Random.shuffle(exceptions).head
        if(this.debugMode) {println("raising "+e+" at "+victim)}        
        victim.untypedRef ! ChaosException(e)
      case NonTerminate =>
        if(this.debugMode) {println("running non-ternimatable calculation at "+victim)}        
        victim.untypedRef ! ChaosNonTerminate
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
    val Random, PoisonKill, Kill, Exception, NonTerminate  = Value
}

@serializable
private[chaos] object Status extends Enumeration {
  type Status = Value
  val ON, OFF = Value
}

class ChaosMonkeyException(message:String) extends Exception(message)
class DefalutChaosException extends Exception("Default ChaosMonkey Exception")