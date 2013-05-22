/*
   Copyright 2012 Jiansen HE

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

package takka.actor

import akka.pattern.ask
// import scala.reflect.runtime.universe._
import scala.reflect.Manifest
import takka.util._
import scala.concurrent.Future


/**
 * An actor reference is an immutable reference to an actor.
 * Only message of type M, or a subtype of M, can be sent via actor reference of type ActorRef[M].
 */
//@SerialVersion("ActorRef-v-0-1")
@SerialVersionUID(13L)
//trait ActorRef[-Msg : TypeTag] { // compile error: traits cannot have type parameters with context bounds
abstract class ActorRef[-M](implicit mt:Manifest[M]) extends Serializable {
  val untypedRef:akka.actor.ActorRef 
//  def typename(implicit m: scala.reflect.TypeTag[M]) = m.toString
  
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
    case that:ActorRef[_] => untypedRef.equals(that.untypedRef)
    case _ => false
  }

  final override def hashCode (): Int = untypedRef.hashCode()

  def !(message: M)(implicit sender: ActorRef[_] = this) = {
    untypedRef.!(message)(sender.untypedRef)
  }
  
  
  override def toString (): String = {
    "ActorRef["+this.mt+"]: "+this.path    
  }
  
  /**
   *  Type safe cast
   */
  def publishAs[SubM<:M](implicit smt:Manifest[SubM]):ActorRef[SubM] = {
    val preciseRef = this.untypedRef
     new ActorRef[SubM] {
      val untypedRef = preciseRef
    } 
  }
  
  /**
   *  Send synchronous request.
   */
  def ?(message: M)(implicit timeout: akka.util.Timeout):Future[Any] = {    
    (untypedRef ? message)
  }
  
  
  /**
   * System Message
   */
  def kill:Unit = {
    untypedRef ! akka.actor.Kill
  } 
  
  def poisonPill:Unit = {
    untypedRef ! akka.actor.PoisonPill
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
//trait SynMessage[M]