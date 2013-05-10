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

import java.io.Closeable
import java.util.concurrent.{ ConcurrentHashMap, ThreadFactory, CountDownLatch, TimeoutException, RejectedExecutionException }
import java.util.concurrent.TimeUnit.MILLISECONDS
import com.typesafe.config.{ Config, ConfigFactory }
import akka.event._
import akka.dispatch._
// import akka.japi.Util.immutableSeq
import akka.actor.dungeon.ChildrenContainer
import akka.util._
import scala.annotation.tailrec
import scala.collection.immutable
import scala.concurrent.duration.{ FiniteDuration, Duration }
import scala.concurrent.{ Await, Awaitable, CanAwait, Future, ExecutionContext }
import scala.util.{ Failure, Success }
import scala.util.control.{ NonFatal, ControlThrowable }

import takka.nameserver._
import akka.pattern.ask
// import scala.reflect.runtime.universe._
import scala.reflect.Manifest

object ActorSystem {
  def apply():ActorSystem = new ActorSystem {
    val system = akka.actor.ActorSystem.apply()
    system.actorOf(akka.actor.Props(new ActorTypeChecker), "ActorTypeChecker")
  }
  def apply(name: String): ActorSystem = new ActorSystem {
    val system = akka.actor.ActorSystem(name)
    system.actorOf(akka.actor.Props(new ActorTypeChecker), "ActorTypeChecker")
  }
  
  def create(name: String): ActorSystem = apply(name)
  def create(name: String, config: Config): ActorSystem = apply(name, config)
  def apply(name: String, config: Config): ActorSystem = new ActorSystem {
//    println(name + "  "+ config +"  ")
    val system = akka.actor.ActorSystem.apply(name, config)
    system.actorOf(akka.actor.Props(new ActorTypeChecker), "ActorTypeChecker")
  }

}

/**
 * An actor system maintains groups of actors in supervision trees.
 * It is also the entry point for creating or looking up actors.
 *
 * There are several possibilities for creating actors (see [[akka.actor.Props]]
 * for details on `props`):
 *
 * {{{
 * system.actorOf(props, "name")
 * system.actorOf(props)
 * }}}
 * Where no name is given explicitly, one will be automatically generated.
 */
abstract class ActorSystem {
  import akka.actor.{Extension, ExtensionId}
  
  val system:akka.actor.ActorSystem
/*
  def name: String = {system.name}
  def logConfiguration(): Unit = system.logConfiguration
  def /(name: String): akka.actor.ActorPath = system./(name)
  def child(child: String): akka.actor.ActorPath = /(child)
  def /(name: Iterable[String]): akka.actor.ActorPath = system./(name)
  def descendant(names: java.lang.Iterable[String]): akka.actor.ActorPath = /(immutableSeq(names))
  val startTime: Long = System.currentTimeMillis
  def uptime: Long = (System.currentTimeMillis - startTime) / 1000
  def eventStream: EventStream = system.eventStream
  def log: LoggingAdapter = system.log
  * 
  */
  
  def stop(actorRef:ActorRef[_]) = {system.stop(actorRef.untypedRef)}
  // use publishas[T] when ActorRef[T] is required
  def deadLetters : ActorRef[Any] = new ActorRef[Any]{
    val untypedRef = system.deadLetters
  }
  //#scheduler
//  def scheduler: Scheduler = system.scheduler
  def dispatchers: Dispatchers = system.dispatchers
  implicit def dispatcher: ExecutionContext = system.dispatcher
  def registerOnTermination[T](code: => T): Unit = system.registerOnTermination(code)
  def registerOnTermination(code: Runnable): Unit = system.registerOnTermination(code)
  def awaitTermination(timeout: Duration): Unit = system.awaitTermination(timeout)
  def awaitTermination(): Unit =  system.awaitTermination
  def shutdown(): Unit = system.shutdown
  def isTerminated: Boolean = system.isTerminated
  def registerExtension[T <: Extension](ext: ExtensionId[T]): T = system.registerExtension(ext)
  def extension[T <: Extension](ext: ExtensionId[T]): T = system.extension(ext)
  def hasExtension(ext: ExtensionId[_ <: Extension]): Boolean = system.hasExtension(ext)

  def settings: akka.actor.ActorSystem.Settings = system.settings
  def name:String = system.name
  
  /**
   * Create a top-level actor, with system generated name.
   */  
  def actorOf[Msg:Manifest](props:Props[Msg]):ActorRef[Msg] = {
    val actor = new ActorRef[Msg] { val untypedRef = system.actorOf(props.props) }    
    NameServer.set(TSymbol[ActorRef[Msg]](scala.Symbol(actor.path.toString())), actor)    
    actor
  }

  /**
   * Create a top-level actor. with user specified name.
   */  
  def actorOf[Msg:Manifest](props:Props[Msg], name:String):ActorRef[Msg] = {
    val actor = new ActorRef[Msg] { val untypedRef = system.actorOf(props.props, name) }
    NameServer.set(TSymbol[ActorRef[Msg]](Symbol(actor.path.toString())), actor)
    actor
  }
  
  
  def scheduler : takka.actor.Scheduler = new takka.actor.Scheduler {
    val akkaScheduler = system.scheduler
    
    def schedule[M](
      initialDelay: FiniteDuration,
      interval: FiniteDuration,
      receiver: ActorRef[M],
      message: M)(implicit executor: ExecutionContext, sender: ActorRef[_] = Actor.noSender): akka.actor.Cancellable = {
      akkaScheduler.schedule(initialDelay, interval, receiver.untypedRef, message)(executor)
    }
    
    def schedule(
      initialDelay: FiniteDuration,
      interval: FiniteDuration)(f: => Unit)(
      implicit executor: ExecutionContext): akka.actor.Cancellable = {
      akkaScheduler.schedule(initialDelay, interval)(f)(executor)
    }

    def schedule(
      initialDelay: FiniteDuration,
      interval: FiniteDuration,
      runnable: Runnable)(implicit executor: ExecutionContext): akka.actor.Cancellable = {
      akkaScheduler.schedule(initialDelay, interval, runnable)(executor)
    }
    
    def scheduleOnce(
      delay: FiniteDuration,
      runnable: Runnable)(implicit executor: ExecutionContext): akka.actor.Cancellable = {
      akkaScheduler.scheduleOnce(delay, runnable)(executor) 
      }
    
    def scheduleOnce[M](
      delay: FiniteDuration,
      receiver: ActorRef[M],
      message: M)(implicit executor: ExecutionContext): akka.actor.Cancellable = {
      akkaScheduler.scheduleOnce(delay, receiver.untypedRef, message)(executor)
    }
    
    def scheduleOnce(
      delay: FiniteDuration)(f: => Unit)(
      implicit executor: ExecutionContext): akka.actor.Cancellable = {
      akkaScheduler.scheduleOnce(delay)(f)(executor)
    }
  }
  
  override def toString():String = system.toString()

  
  // Concrete Value Members
  
  // actorFor  via nameserver !!!	
  // TODO: E is not checked
  def actorFor[M:Manifest](actorPath: String): ActorRef[M]= {
    
    //val isRemotePath = ActorPath(actorPath)
    val tmp = new ActorRef[M]{
      val untypedRef = system.actorFor(actorPath)
    }
    actorFor[M](tmp.path)
  }
  
  def actorFor[M:Manifest](actorPath: akka.actor.ActorPath): ActorRef[M]= {
    val isRemotePath = actorPath.address.host match {
      case None => false
      case Some(_) => true
    }
    
    if (isRemotePath) {
      // remote actor reference, fetch from remote name server
      val untyped_ref:akka.actor.ActorRef = system.actorFor(actorPath)
      // check type ...
      // connect to the type server of a remote system
      
      //"akka://CalculatorApplication@127.0.0.1:2552/user/simpleCalculator"
      val remoteChecker:akka.actor.ActorRef  = system.actorFor("akka://"+actorPath.address.system+"@"
                              +actorPath.address.host.get+":"
                              +actorPath.address.port.get+"/user/ActorTypeServer")
      implicit val timeout = new akka.util.Timeout(10000) // 10 seconds
      val checkResult = remoteChecker ? Check(actorPath, manifest[M]) 
      var result:ActorRef[M] = null
      checkResult onSuccess {
        case Compatible => 
          result = new ActorRef[M]{
            val untypedRef = system.actorFor(actorPath)
          } 
        case NonCompatible => 
          throw new Exception("ActorRef["+actorPath+"] does not exist or does not have type ActorRef["+manifest[M]+"]")
      }
      result
    }else{
      // local actor reference, fetch from local name server
      new ActorRef[M]{
        val untypedRef = system.actorFor(actorPath)
      }    
    }
  }
    
  //  new APIs to support remote ActorRef  
  // TODO: may need to modify when akka 2.1 release (Cluster instead of akka) 
  private def getHostname():String = {
    system.settings.config.getConfig("akka").getConfig("remote").getConfig("netty").getValue("hostname").render()
  }
  
  def isLocalSystem():Boolean = {
    val host = getHostname()
    host == "\"\"" || host.startsWith("\"127.")
  }
  
  @throws(classOf[NotRemoteSystemException])
  def host:String = {
    if (isLocalSystem) {
      throw NotRemoteSystemException(this)      
    }else{
      getHostname      
    }
  }
  
  private def getPortnumber():Int = {
    system.settings.config.getConfig("akka").getConfig("remote").getConfig("netty").getValue("port").render().toInt
  }
  
  @throws(classOf[NotRemoteSystemException])
  def port:Int = {
    if (isLocalSystem) {
      throw NotRemoteSystemException(this)
    }else{
      getPortnumber      
    }
  }
  
  def remoteActorOf[Msg:Manifest](props:Props[Msg]):ActorRef[Msg] = {
    val actor = actorOf[Msg](props:Props[Msg])
    val system = this
    new ActorRef[Msg] {
      val localPathStr = actor.path.toString()
      val sys_path = localPathStr.split("@")
      val remotePathStr = sys_path(0)+"@"+system.host+":"+system.port+sys_path(1)
//akka://RemoteCreation@129.215.91.195:2554/user/...
      val untypedRef = system.system.actorFor(remotePathStr)
    }
  }
  
  def remoteActorOf[Msg:Manifest](props:Props[Msg], name:String):ActorRef[Msg] = {
    val actor = actorOf[Msg](props:Props[Msg], name:String)
    val system = this
    new ActorRef[Msg] {
      val localPathStr = actor.path.toString()
      val sys_path = localPathStr.split("@")
      val remotePathStr = sys_path(0)+"@"+system.host+":"+system.port+sys_path(1)
//akka://RemoteCreation@129.215.91.195:2554/user/...
      val untypedRef = system.system.actorFor(remotePathStr)
    }
  }
  
  import language.existentials
//  private sealed trait ActorTypeCheckerMsg
  private case class Check(path:akka.actor.ActorPath, t:Manifest[_]) 
  private case object Compatible
  private case object NonCompatible // type error or isDeadLetter

  
  private class ActorTypeChecker extends akka.actor.Actor{
    def receive = {
      case Check(path, t) =>
        NameServer.get(TSymbol(Symbol(path.toString))(t) ) match {
          case None => sender ! NonCompatible // not registered actor or incompatible type
          case Some(_) => sender ! Compatible
        }
    }
  }
}

case class NotRemoteSystemException(system:ActorSystem) extends Exception("ActorSystem: "+system+" does not support remoting")