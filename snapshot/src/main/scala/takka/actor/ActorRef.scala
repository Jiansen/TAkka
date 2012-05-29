package takka.actor

import takka.util.SerialVersionUID

@serializable
@SerialVersionUID("ActorRef-v-0-1")
//trait ActorRef[-Msg : Manifest] { // compile error: traits cannot have type parameters with context bounds
abstract class ActorRef[-M] {
  val untyped_ref:akka.actor.ActorRef 
  def typename[M](implicit m: scala.reflect.Manifest[M]) = m.toString
  
  // Is the actor shut down? 
  def isTerminated : Boolean = {untyped_ref.isTerminated}
  
  // Return the ActorPath of the actor reference
  def path : akka.actor.ActorPath = {untyped_ref.path}
  
  // Comparing address (akka) and Static Type (this extension)
  final def compareTo (other: ActorRef[_]): Int = {untyped_ref.compareTo(other.untyped_ref)}
  
  //TODO
  override def equals (that: Any):Boolean = that match {
    case that:ActorRef[Any] => untyped_ref.equals(that.untyped_ref)
    case _ => false
  }

  final override def hashCode (): Int = untyped_ref.hashCode()

  //Sends the specified message to the sender
  final def tell (msg: M): Unit = {
    untyped_ref.tell(msg)
  }
  
  // message sending, same as tell
  def ![M](message: M)(implicit sender: ActorRef[M] = this) = {
    untyped_ref.!(message)(sender.untyped_ref)
  }
  
  override def toString (): String = {
    "ActorRef["+this.typename+"]: "+this.path    
  }
  
  // Type safe cast
  def publishAs[SubM<:M]:ActorRef[SubM] = {
    val preciseRef = this.untyped_ref
     new ActorRef[SubM] {
      val untyped_ref = preciseRef
    } 
  }
}