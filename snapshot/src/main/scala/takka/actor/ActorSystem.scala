package takka.actor

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import akka.actor.{ActorPath, Extension, ExtensionId}
import akka.event.LoggingAdapter
import akka.util.Duration
import akka.pattern._

import takka.nameserver.{NameServer, TSymbol}

object ActorSystem {
  def apply():ActorSystem = new ActorSystem {
    val system = akka.actor.ActorSystem.apply()
  }
  def apply(name: String): ActorSystem = new ActorSystem {
    val system = akka.actor.ActorSystem.apply(name)
  }
  
  def create(name: String): ActorSystem = apply(name)
  def create(name: String, config: Config): ActorSystem = apply(name, config)
  def apply(name: String, config: Config): ActorSystem = new ActorSystem {
//    println(name + "  "+ config +"  ")
    val system = akka.actor.ActorSystem.apply(name, config)
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
 *
 * Where no name is given explicitly, one will be automatically generated.
 */
abstract class ActorSystem {
  val system:akka.actor.ActorSystem

  system.actorOf(akka.actor.Props[ActorTypeChecker])
  // Abstract Value Members
  def / (name: Iterable[String]): ActorPath = {
    system / name
  }
  
  def / (name: String): ActorPath = {
    system / name
  }
  
  /**
   * Create a top-level actor, with system generated name.
   */  
  def actorOf[Msg:Manifest](props:Props[Msg]):ActorRef[Msg] = {
    val actor = new ActorRef[Msg] { val untyped_ref = system.actorOf(props.props) }
    
    NameServer.set(TSymbol[ActorRef[Msg]](Symbol(actor.path.toString())), actor)
    
    actor
  }

  /**
   * Create a top-level actor. with user specified name.
   */  
  def actorOf[Msg:Manifest](props:Props[Msg], name:String):ActorRef[Msg] = {
    val actor = new ActorRef[Msg] { val untyped_ref = system.actorOf(props.props, name) }
    NameServer.set(TSymbol[ActorRef[Msg]](Symbol(actor.path.toString())), actor)
    actor
  }
  
  def awaitTermination (): Unit = {
    system.awaitTermination()
  }
  
  def awaitTermination (timeout: Duration): Unit = {
    system.awaitTermination(timeout)
  }
  
  def eventStream : akka.event.EventStream = {
    system.eventStream
  }
  
  
  def extension [T <: Extension] (ext: ExtensionId[T]): T = {
    system.extension(ext)
  }
  
  // Return true if the system is shutdown
  def isTerminated : Boolean = { system.isTerminated }
  
  def log : LoggingAdapter =  { system.log }
  
  def logConfiguration (): Unit = { system.logConfiguration() }
  
//  def name : String = { system.name }
  
  def registerExtension [T <: Extension] (ext: ExtensionId[T]): T = {
    system.registerExtension(ext)
  }
  
  def registerOnTermination (code: Runnable): Unit = {
    system.registerOnTermination(code)
  }
  
  def registerOnTermination [T] (code: => T): Unit = {
    system.registerOnTermination(code)
  }
  
  def scheduler : akka.actor.Scheduler = {
    system.scheduler
  }
  
  def settings : akka.actor.ActorSystem.Settings = {system.settings}
  
  def shutdown (): Unit = system.shutdown()
  
  override def toString():String = system.toString()
  
  // TODO: refine followings
  def deadLetters : ActorRef[Any] = new ActorRef[Any]{
    val untyped_ref = system.deadLetters
  }

  
  // Concrete Value Members
  
  // actorFor  via nameserver !!!	
  // TODO: E is not checked
  def actorFor[M:Manifest](actorPath: String): ActorRef[M]= {
    
    //val isRemotePath = ActorPath(actorPath)
    val tmp = new ActorRef[M]{
      val untyped_ref = system.actorFor(actorPath)
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
            val untyped_ref = system.actorFor(actorPath)
          } 
        case NonCompatible => 
          throw new Exception("ActorRef["+actorPath+"] does not exist or does not have type ActorRef["+manifest[M]+"]")
      }
      result
    }else{
      // local actor reference, fetch from local name server
      new ActorRef[M]{
        val untyped_ref = system.actorFor(actorPath)
      }    
    }
  }
    
//  val startTime : Long = system.startTime
  
  def uptime : Long = system.uptime
  
  
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
      val untyped_ref = system.system.actorFor(remotePathStr)
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
      val untyped_ref = system.system.actorFor(remotePathStr)
    }
  }
  
  
//  private sealed trait ActorTypeCheckerMsg
  private case class Check(path:akka.actor.ActorPath, manifest:Manifest[_]) 
  private case object Compatible
  private case object NonCompatible // type error or isDeadLetter

  
  private class ActorTypeChecker extends akka.actor.Actor{
    def receive = {
      case Check(path, manifest) =>
        NameServer.get(TSymbol(Symbol(path.toString))(manifest) ) match {
          case None => sender ! NonCompatible // not registered actor or incompatible type
          case Some(_) => sender ! Compatible
        }
    }
  }
}

case class NotRemoteSystemException(system:ActorSystem) extends Exception("ActorSystem: "+system+" does not support remoting")