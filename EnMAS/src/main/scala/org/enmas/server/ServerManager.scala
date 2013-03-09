package org.enmas.server

import org.enmas.pomdp._, org.enmas.messaging._,
       org.enmas.util.FileUtils._, org.enmas.util.voodoo.ClassLoaderUtils._,
       akka.actor._, akka.actor.Actor._,
       com.typesafe.config.ConfigFactory,
       java.io.File

class ServerManager extends Actor with Provisionable {
  import ServerManager._

  var JARs = List[File]()
  var POMDPs = Set[POMDP]()
  var servers = List[ServerSpec]()

  /** Creates an akka.actor.ActorRef corresponding to a new Server
    * simulating the specified POMDP and listening on the specified port.
    */
  def createServer(className: String) {
    POMDPs.find(_.getClass.getName == className) match {
      case Some(pomdp)  ⇒ {
        val ref = system.actorOf(Props(new Server(pomdp)))
        servers ::= ServerSpec(ref, pomdp.getClass.getName, pomdp.name, pomdp.description)
        sender ! ref // TODO: how ref is used?  It is discarded by ClientManager
      }
      case None  ⇒ ()
    }
  }

  /** Destroys the specified server.
    */
  def stopServer(ref: ActorRef) {
    servers filter { _.ref == ref } map { s  ⇒ context stop s.ref }
    servers = servers filterNot { _.ref == ref }
  }

  def receive = {
    case Discovery  ⇒ sender ! DiscoveryReply(servers)
    case Provision(fileData)  ⇒ {
      val jarOption = provision[POMDP](fileData)
      jarOption map { jar  ⇒ {
        JARs ::= jar
        POMDPs ++= findSubclasses[POMDP](jar) filterNot {
          _.getName contains "$"} map { clazz  ⇒ clazz.newInstance }
      }}
    }
    case RequestProvisions  ⇒ {
      JARs map { jarFile  ⇒ readFile(jarFile) match {
        case Some(fd)  ⇒ sender ! Provision(fd)
        case None  ⇒ ()
      }}
    }
    case CreateServerFor(className)  ⇒ createServer(className)
    case _  ⇒ ()
  }
}

object ServerManager extends App {
  println(new File(".").getAbsolutePath)
  val system = ActorSystem("enmasServer", ConfigFactory.load.getConfig("enmasServer"))
  val manager = system.actorOf(Props[ServerManager], "serverManager")
}