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

import scala.concurrent.duration.Duration
import scala.reflect.runtime.universe._
import language.implicitConversions
import takka.chaos._
import akka.actor.PoisonPill

/**
 * The actor context - the internal view of the actor cell from the actor itself.
 * 
 * TAkka encourages functional message processing, that is, 
 * if the process of a message requires certain information, that information should
 * be part of the message.
 *
 * There are several possibilities for creating actors (see [[takka.actor.Props]]
 * for details on `props`):
 *
 * {{{
 * context.actorOf(props, "name")
 * context.actorOf(props)
 *
 * // Scala
 * context.actorOf(Props[M, MyActor]("name")
 * context.actorOf(Props[M, MyActor]
 * context.actorOf(Props[M](new MyActor(...))
 * }}}
 * Where no name is given explicitly, one will be automatically generated.
 */
abstract class ActorContext[M:TypeTag] {
//  implicit val m:TypeTag[M] = typeTag[M]
  implicit var mt : Type = typeOf[M]

  val untyped_context:akka.actor.ActorContext

  val props:Props[M]
  
  def actorOf[Msg](props:Props[Msg], name:String)(implicit mt:TypeTag[Msg]):ActorRef[Msg] = {
    new ActorRef[Msg] { val untypedRef = untyped_context.actorOf(props.props, name) }
  }
  
  def actorOf[Msg](props:Props[Msg])(implicit mt:TypeTag[Msg]):ActorRef[Msg] = {
    new ActorRef[Msg] { val untypedRef = untyped_context.actorOf(props.props) }
  }
  
  def receiveTimeout : Duration = {
    untyped_context.receiveTimeout
  }
  
  lazy val typedSelf:ActorRef[M] = new ActorRef[M]{
    val untypedRef = untyped_context.self
  }
  
  def setReceiveTimeout (timeout: Duration): Unit = {
    untyped_context.setReceiveTimeout(timeout)
  }
  
  def stop (actor: ActorRef[_]): Unit = {
    untyped_context.stop(actor.untypedRef)
  }
  
  private var internalSystem:ActorSystem = null
  implicit def system : ActorSystem = {
    if (internalSystem == null){
      internalSystem = new ActorSystem {
        val system = untyped_context.system
      }
    }
    internalSystem
  }
  
  def unwatch[Msg](subject: ActorRef[Msg]): ActorRef[Msg] = {
    untyped_context.unwatch(subject.untypedRef)
    subject    
  }
  
  def watch[Msg](subject: ActorRef[Msg]): ActorRef[Msg] = {
    untyped_context.watch(subject.untypedRef)
    subject    
  }
    
  // actorFor  via nameserver !!!	
  // TODO: Msg is not checked
  def actorFor[Msg](actorPath: String)(implicit mt:TypeTag[Msg]): ActorRef[Msg]= new ActorRef[Msg]{
    val untypedRef = untyped_context.actorFor(actorPath)
  }
  
  //  new APIs to support remote ActorRef
  def remoteActorOf[Msg](props:Props[Msg])(implicit mt:TypeTag[Msg]):ActorRef[Msg] = {
    val actor = actorOf[Msg](props:Props[Msg])
    val system = this.system;
    new ActorRef[Msg] {
      val localPathStr = actor.path.toString()
      val sys_path = localPathStr.split("@")
      val remotePathStr = sys_path(0)+"@"+system.host+":"+system.port+sys_path(1)
//akka://RemoteCreation@129.215.91.195:2554/user/...
      val untypedRef = system.system.actorFor(remotePathStr)
    }
  }
  
  def remoteActorOf[Msg](props:Props[Msg], name:String)(implicit mt:TypeTag[Msg]):ActorRef[Msg] = {
    val actor = actorOf[Msg](props:Props[Msg], name:String)
    val system = this.system
    new ActorRef[Msg] {
      val localPathStr = actor.path.toString()
      val sys_path = localPathStr.split("@")
      val remotePathStr = sys_path(0)+"@"+system.host+":"+system.port+sys_path(1)
//akka://RemoteCreation@129.215.91.195:2554/user/...
      val untypedRef = system.system.actorFor(remotePathStr)
    }
  }
  
  def become[SupM >: M](behavior: SupM => Unit, possibleHamfulHandler:akka.actor.PossiblyHarmful => Unit)(implicit smtTag:TypeTag[SupM]):ActorRef[SupM] = {
//  def become[SupM, M <: SupM](behavior: SupM => Unit, possibleHamfulHandler:akka.actor.PossiblyHarmful => Unit):Unit = {
    val smt = typeOf[SupM]
    if (!(mt <:< smt))
      throw BehaviorUpdateException(smt, mt)
    mt = smt
    untyped_context.become({
      case x:akka.actor.PossiblyHarmful => possibleHamfulHandler(x)   
      case x:ChaosMessage => chaosHandler(x)
      case x:SupM => behavior(x)
    })
    new ActorRef[SupM] {
      val untypedRef = untyped_context.self
    }
  }
  
    /**
   *  private handler for chaos message
   */
  private[actor] def chaosHandler:ChaosMessage => Unit = {
    case Propagation(r) => 
//      println("Propagation Received: "+self)
      assert (r>=0 && r<=1, {throw new ChaosException("Propagation ratio "+r+"must be bewtten 0 and 1 inclusively")})
      val random = new scala.util.Random
      for (c <- untyped_context.children){
        if (random.nextFloat < r){
          c ! PoisonPill
        }else{
          c ! Propagation(r)
        }
      }
  }
}

case class BehaviorUpdateException(smt:Type, mt:Type) extends Exception(smt + "must be a supertype of "+mt+".")