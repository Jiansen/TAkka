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

// import scala.reflect.runtime.universe._
import scala.reflect.Manifest
import takka.chaos._
import takka.supervisionview.SupervisionViewRequest
  
/**
 * A stronger typed Actor trait based on akka.actor.Actor.
 * 
 * A typed actor definition takes a type parameter (M) denoting the type of the actor's initial message loop (M => Unit).
 *
 * An actor is either alive or dead.
 * -- an alive actor can receive and handle message of the right type.
 * -- a dead actor cannot handle any message.  When an actor is dying, a death notafication will be sent to its supervisor and linked actors.
 *
 * The Actor's own [[takka.actor.ActorRef]] is available as `typedSelf`,
 *  [[takka.actor.ActorContext]] as `typedContext`. 
 * The only abstract method is `typedReceive` which shall return the
 * initial behavior of the actor as a partial function (behavior can be changed
 * using `typedContext.become`).
 *
 * Following example is adapted from akka documentation [[http://doc.akka.io/api/akka/2.0.1/#akka.actor.Actor]]
 *
 * {{{
 * sealed trait Message
 * sealed trait Result
 * case class Request(j:Job, reply:Actor[Result]) extends Message
 * case object Shutdown extends Message
 * case class Dangerous(j:Job, sender:Worker) extends Message
 * case class OtherJob(j:Job, sender:Worker) extends Message
 * case class JobReply(result:Result, orig_s:Actor[Result]) extends Message
 * 
 * class ExampleActor extends Actor[Message] {
 *
 *   override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
 *     case _: ArithmeticException      => Resume
 *     case _: NullPointerException     => Restart
 *     case _: IllegalArgumentException => Stop
 *     case _: Exception                => Escalate
 *   }
 *
 *   def typedReceive = {
 *                                      // directly calculated reply
 *     case Request(j, reply)        => reply ! calculate(j)
 *
 *                                      // just to demonstrate how to stop yourself
 *     case Shutdown                 => typedContext.stop(typedSelf)
 *
 *                                      // error kernel with child replying directly to ���customer���
 *     case Dangerous(j, sender)     => typedContext.actorOf(Props[ReplyToOriginWorker]).tell(PerformWork(j), sender)
 *
 *                                      // error kernel with reply going through us
 *     case OtherJob(j, sender)      => context.actorOf(Props[ReplyToMeWorker]) ! JobRequest(j, sender)
 *     case JobReply(result, orig_s) => orig_s ! result
 *   }
 * }
 * }}}
 *
 * The Dangerous and OtherJob cases demonstrate the essence of the error kernel design: spawn
 * one-off actors which terminate after doing their job, pass on `sender` to
 * allow direct reply if that is what makes sense, or round-trip the sender
 * as shown with the fictitious JobRequest/JobReply message pair.
 *
 * If you don���t like writing `typedContext` you can always `import typedContext._` to get
 * direct access to `actorOf`, `stop` etc. This is not default in order to keep
 * the name-space clean.
 */
//trait Actor[-Msg] extends akka.actor.Actor {
//trait Actor[Msg](implicit msgT:TypeTag[Msg]) extends akka.actor.Actor {
//trait Actor[Msg:TypeTag] extends akka.actor.Actor {
//trait TypedActor[M] extends akka.actor.Actor{  
abstract class TypedActor[M:Manifest] extends akka.actor.Actor{  
//  implicit val mt:TypeTag[M] = typeTag[M]
  /**
   * This defines the initial actor behavior, it must return a partial function
   * with the actor logic.
   */
  protected def typedReceive:PartialFunction[M, Unit]
  
  def receive = {
    case hmsg:akka.actor.PossiblyHarmful => hmsg match {
      case akka.actor.ReceiveTimeout => systemMessageHandler(ReceiveTimeout)   
      case m => possiblyHarmfulHandler(m)
    }
    case chaos:ChaosMessage => typedContext.chaosHandler(chaos)
    case chartMsg:SupervisionViewRequest => typedContext.chartHandler(chartMsg)
    case m:M => typedReceive(m)
//    case x => throw new Exception(typedSelf+": message "+x+" has the wrong type.")
  }

   /**
   * Stores the context for this actor, including typedSelf
   */
  protected[actor] implicit val typedContext:ActorContext[M] = new ActorContext[M](){
     val untypedContext = context
     val props:Props[M] = Props(untypedContext.props)
  }
  
  /**
   * The 'typedSelf' field holds the ActorRef[M] for this actor,
   * which typically cannot be used outside the local machine.
   * <p/>
   * Can be used to send messages to itself:
   * <pre>
   * typedSelf ! message
   * </pre>
   */
  implicit final val typedSelf:ActorRef[M] = new ActorRef[M] {
    val untypedRef = self
  } 
  
  /**
   * The 'typedRemoteSelf' field holds the ActorRef[M] for this actor,
   * which can be used across the network, if the ActorSystem where
   * this actor locate in listens to an active port.
   */
  final lazy val typedRemoteSelf:ActorRef[M] =  {
    val system = typedContext.system
    new ActorRef[M] {
      val localPathStr = self.path.toString()
      val sys_path = localPathStr.split("@")
      val remotePathStr = sys_path(0)+"@"+system.host+":"+system.port+sys_path(1)
//akka://RemoteCreation@129.215.91.195:2554/user/creationActor    
      val untypedRef = context.actorFor(remotePathStr)
    } 
  } 
  // def receiveTimeout : () => Unit = () => {}
  
  /**
   * handler of system messages
   * 
   * Currently the only subclass of SystemMessage is ReceiveTimeout
   * 
   * Other internal messages for supervision purposes are inviable to general users.
   */
  def systemMessageHandler:SystemMessage => Unit = {
    case _ => 
  }
  
  def possiblyHarmfulHandler:akka.actor.PossiblyHarmful => Unit = { case _ =>  }

}


object Actor {
  final val noSender: ActorRef[_] = null
}

object Status {
  sealed trait Status extends Serializable

  /**
   * This class/message type is preferably used to indicate success of some operation performed.
   */
  @SerialVersionUID(1L)
  case class Success(status: AnyRef) extends Status

  /**
   * This class/message type is preferably used to indicate failure of some operation performed.
   * As an example, it is used to signal failure with AskSupport is used (ask/?).
   */
  @SerialVersionUID(1L)
  case class Failure(cause: Throwable) extends Status
}