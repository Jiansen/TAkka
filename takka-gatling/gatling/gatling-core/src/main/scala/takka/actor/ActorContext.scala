/**
 * Copyright 2011-2012 eBusiness Information, Groupe Excilys (www.excilys.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * 		http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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

import akka.util.Duration

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
trait ActorContext[M] {
  implicit var mt:Manifest[M] = manifest[M]

  val untyped_context:akka.actor.ActorContext

  val props:Props[M]
  
  def actorOf[Msg](props:Props[Msg], name:String)(implicit mt:Manifest[Msg]):ActorRef[Msg] = {
    new ActorRef[Msg] { val untypedRef = untyped_context.actorOf(props.props, name) }
  }
  
  def actorOf[Msg](props:Props[Msg])(implicit mt:Manifest[Msg]):ActorRef[Msg] = {
    new ActorRef[Msg] { val untypedRef = untyped_context.actorOf(props.props) }
  }
  
  def receiveTimeout : Option[Duration] = {
    untyped_context.receiveTimeout
  }
  
  def resetReceiveTimeout (): Unit = {
    untyped_context.resetReceiveTimeout()
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
  def actorFor[Msg](actorPath: String)(implicit mt:Manifest[Msg]): ActorRef[Msg]= new ActorRef[Msg]{
    val untypedRef = untyped_context.actorFor(actorPath)
  }
  
  //  new APIs to support remote ActorRef
  def remoteActorOf[Msg](props:Props[Msg])(implicit mt:Manifest[Msg]):ActorRef[Msg] = {
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
  
  def remoteActorOf[Msg](props:Props[Msg], name:String)(implicit mt:Manifest[Msg]):ActorRef[Msg] = {
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
  
  def become[SupM >: M](behavior: SupM => Unit, possibleHamfulHandler:akka.actor.PossiblyHarmful => Unit)(implicit smt:Manifest[SupM]):ActorRef[SupM] = {
//  def become[SupM, M <: SupM](behavior: SupM => Unit, possibleHamfulHandler:akka.actor.PossiblyHarmful => Unit):Unit = {
    if (!(smt >:> mt))
      throw BehaviorUpdateException(smt, mt)
    
    untyped_context.become({
      case x:SupM => behavior(x)
      case x:akka.actor.PossiblyHarmful => possibleHamfulHandler(x)
    })
    new ActorRef[SupM] {
      val untypedRef = untyped_context.self
    }
  }
}

case class BehaviorUpdateException(smt:Manifest[_], mt:Manifest[_]) extends Exception(smt + "must be a supertype of "+mt+".")