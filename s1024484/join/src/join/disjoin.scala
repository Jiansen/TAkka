package join

/** A module supporting distributed join calculus.
 * 
 *  @author  Jiansen HE
 *  @version 0.1, 15/08/2011
 */

import scala.collection.mutable.HashMap

import scala.actors.{Actor, OutputChannel}
import scala.actors.Actor._
import scala.actors.AbstractActor
import scala.actors.remote.RemoteActor
import scala.actors.remote.RemoteActor._
import scala.actors.remote.Node

import java.net.URL
import java.net.URLClassLoader

import scala.Symbol
import scala.reflect.Manifest

case class JoinMessage(name : String, arg:Any)
//check the type of a distributed synchronous name
case class SynNameCheck[Arg, R](name : String, argT:ClassManifest[Arg], resT:ClassManifest[R])
//check the type of a distributed asynchronous name
case class AsyNameCheck[Arg](name : String, argT:ClassManifest[Arg])
//channel name is not registered in current site
case class NameNotFound

/** Distributed join definition
 * 
 *  @param port: the communication port
 *  @param name: the name of this join definition
 */
class DisJoin(port:Int, name: Symbol) extends Join with Actor{
  //work as name server
  var channelMap = new HashMap[String, NameBase]
  
  /** register a local channel with a name
   *
   *  @param  name  global name of the local channel
   *  @param  ch    the local channel 
   */
  def registerChannel(name:String, ch:NameBase){
    assert(!channelMap.contains(name), name+" has been registered.")
    channelMap += ((name, ch))
  }  
  
  def act(){
    alive(port)
    register(name, self)
    println("Server "+name+" Started...")    
    
    loop(
      react { 
        case JoinMessage(name, arg:Any) => {
          if (channelMap.contains(name)) {
            channelMap(name) match {
              case n : SynName[Any, Any] => 
                println("before calc")
                val r = n(arg)
                println("after calc")
                sender ! n(arg)
                println("result sended")
              case n : AsyName[Any] => n(arg)
            }
          }
        }
        case SynNameCheck(name, argT, resT) => {
          if (channelMap.contains(name)) {
            sender ! (channelMap(name).argTypeEqual((argT,resT)))
          }else{
            sender ! NameNotFound
          }
        }
        case AsyNameCheck(name, argT) => {
          if (channelMap.contains(name)) {
            sender ! (channelMap(name).argTypeEqual(argT))
          }else{
            sender ! NameNotFound
          }
        }
      }
    )
  }
}

/** Distributed synchronous name (at user side)
 * 
 *  @param Arg:   descriptor of argument type
 *  @param R:     descriptor of argument type
 *  @param owner: the join definition where the channel is defined 
 */
class DisSynName[Arg:Manifest, R:Manifest](n:String, owner:scala.actors.AbstractActor){
  val argT = implicitly[ClassManifest[Arg]]//type of arguments
  val resT = implicitly[ClassManifest[R]]//type of return value
  
  initial()// type checking etc.
  
  // sending message via distributed channel
  def apply(arg:Arg) :R = synchronized {
    owner ! JoinMessage(n, arg)
    receive {
      case r => println("received"); r.asInstanceOf[R]
    }
  }
  
  //check type etc.
  def initial() = synchronized {
    (owner !? SynNameCheck(n, argT, resT)) match {
      case true => Unit
      case false => throw new Error("Distributed channel initial error: Channel \"" + n + "\" does not have type "+argT+ " => "+resT+".")
      case NameNotFound => throw new Error("name "+n+" is not found at "+owner)
    }
  }
} 

/** Distributed asynchronous name (at user side)
 * 
 *  @param Arg:   descriptor of argument type
 *  @param owner: the join definition where the channel is defined 
 */
class DisAsyName[Arg:Manifest](n:String, owner:scala.actors.AbstractActor){
  val argT = implicitly[ClassManifest[Arg]]
  
  initial()
  
  // sending message via distributed channel
  def apply(arg:Arg) = synchronized {
    (owner ! JoinMessage(n, arg))
  }
  
  //check type etc.
  def initial() = synchronized {
    (owner !? AsyNameCheck(n, argT)) match {
      case true => Unit
      case false => throw new Error("Distributed channel initial error: Channel \"" + n + "\" does not take argument of type "+argT+".")
      case NameNotFound => throw new Error("name "+n+" is not found at "+owner)
    }
  }
  
}

// connect to a distributed join definition
object DisJoin {
  def connect(addr:String, port:Int, name:Symbol):AbstractActor = {
    val peer = Node(addr, port)//location of the server
    RemoteActor.select(peer, name)
  }
}