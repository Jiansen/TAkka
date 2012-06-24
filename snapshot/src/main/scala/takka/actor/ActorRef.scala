package takka.actor

import akka.pattern.ask
import takka.util.SerialVersionUID


/**
 * An actor reference is an immutable reference to an actor.
 * Only message of type M, or a subtype of M, can be sent via actor reference of type ActorRef[M].
 */

@serializable
@SerialVersionUID("ActorRef-v-0-1")
//trait ActorRef[-Msg : Manifest] { // compile error: traits cannot have type parameters with context bounds
abstract class ActorRef[-M](implicit mt:Manifest[M]) {
  val untypedRef:akka.actor.ActorRef 
//  def typename(implicit m: scala.reflect.Manifest[M]) = m.toString
  
  /**
   *  Return true if the actor has been shut down 
   */
  def isTerminated : Boolean = {untypedRef.isTerminated}
  
  /**
   *  Return the ActorPath of the actor reference
   */
  def path : akka.actor.ActorPath = {untypedRef.path}
  
  /**
   *  Comparing address (akka) and Static Type (extended feature)
   */
  final def compareTo (other: ActorRef[_]): Int = {untypedRef.compareTo(other.untypedRef)}
  
  override def equals (that: Any):Boolean = that match {
    case that:ActorRef[Any] => untypedRef.equals(that.untypedRef)
    case _ => false
  }

  final override def hashCode (): Int = untypedRef.hashCode()

  /**
   * Sends message:M to the actor reference.
   */
  final def tell (msg: M): Unit = {
    untypedRef.tell(msg)
  }
  
  /**
   *  Sends message:M to the actor reference, same as tell.
   */
//  def !(message: M) = {
//    untypedRef ! message
//  }
  
  def !(message: M)(implicit sender: ActorRef[_] = this) = {
    untypedRef.!(message)(sender.untypedRef)
  }
  
  
  override def toString (): String = {
    "ActorRef["+this.mt+"]: "+this.path    
  }
  
  /**
   *  Type safe cast
   */
  def publishAs[SubM<:M](implicit mt:Manifest[SubM]):ActorRef[SubM] = {
    val preciseRef = this.untypedRef
     new ActorRef[SubM] {
      val untypedRef = preciseRef
    } 
  }
  
  /**
   *  Send synchronous request.  The current implementation is buggy because it does not restrict SynMessage[R] <: M
   */
  def ?[R](message: SynMessage[R])(implicit timeout: akka.util.Timeout, mr:Manifest[R]):akka.dispatch.Future[R] = {    
    (untypedRef ? message).mapTo[R]
  }
}


/**
 *  Synchronous message that waits for a reply of type M.
 *  {{{
sealed trait Msg
case class MyString(str:String) extends SynMessage[Int] with Msg
case class StrMsg extends SynMessage[Int] with Msg
  
object SynTest extends App{  
  class Act extends Actor[Msg] {
    def typedReceive = {
      case MyString("Hello") => 
    }
  }
  
  val system = ActorSystem("FooSystem")
  val actor = system.actorOf(Props[Msg, Act])
  
  import akka.util.Timeout
  import akka.util.duration._	
  implicit val timeout = Timeout(1 second)
  
  actor.?[Int](MyString("Hello")) onSuccess {
    //case 2.1 => println("Received Double") // Scala bug
    case m:Int => println("Received Int "+  m)
    // case "Bong"  => println("string received.") // type mismatch; found : java.lang.String("Bong") required: Int
  }
}
 *  }}}
 */
trait SynMessage[M]