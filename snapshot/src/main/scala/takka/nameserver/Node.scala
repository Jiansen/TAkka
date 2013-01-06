package takka.nameserver

import util._

import akka.actor.{ ActorRef, Props, Actor, ActorSystem }
import com.typesafe.config.ConfigFactory
import akka.pattern.ask // for synchronous ask operator (?)
import akka.util.Timeout
import scala.concurrent.duration.Duration

/*
 * A node in an distributed system
 * 
 *   returns local values to remote nodes
 *   
 */

/*
trait NodeEnquiry
case class getValue(vSymbol:Symbol, vType:Manifest[_])

class SimpleNodeActor extends Actor {
  def receive = {
    case getValue(vSymbol:TSymbol, vType:Manifest[_]) =>
      val res = NameServer.get(vSymbol)(vType) match {
        case None => SNone
        case Some(m) => SSome(m)
      }
      sender ! res
  }
}

// start a local node 
//   address should be a global hostname or ip
// TODO:  let address be 'localhost'
class LocalNode(address:String, port:Int) {
  val system = ActorSystem("NodeSystem",
               ConfigFactory.parseString("""
                 akka{
                   actor {
                     provider = "akka.remote.RemoteActorRefProvider"
                   }

                   remote {
                     netty {                       
                       hostname = """ + "\""+address + "\""+ """
                       port = """ + port + """         
                     }
                   }
                 }""") )
  // hostname = "127.0.0.1"
  val actor = system.actorOf(Props[SimpleNodeActor], "Node")
  //println(actor)
}

class RemoteNode(address:String, port:Int){  
  val system = ActorSystem("RemoteNodeSystem", 
                           ConfigFactory.parseString(""" 
                 akka{
                   actor {
                     provider = "akka.remote.RemoteActorRefProvider"
                   }

                   remote {
                     netty {
                       hostname = "127.0.0.1"
                       remote.netty.port = 2553
                     }
                   }  
                 }""") )
  val remoteNodeActor = system.actorFor("akka://NodeSystem@"+address+":"+port+"/user/Node")
  
  // reference to remote node
  def get[T](name:Symbol)(implicit m: Manifest[T]):SOption[T] = {
    implicit val timeout = Timeout(5 seconds)
    val resF = (remoteNodeActor ? getValue(name, m)).mapTo[SOption[T]]
    Await.result(resF, 1 second)
  }
}


/*
object LocalNodeTest extends App{
  new LocalNode("129.215.91.88", 2559)
  println("Node stated")
  NameServer.set('anInt, 2)
}
*/

object remoteNodeTest extends App{
  val node = new RemoteNode("129.215.91.88", 2559)
  
  val anInt = node.get[Int]('anInt)
  println(anInt)
}
*/