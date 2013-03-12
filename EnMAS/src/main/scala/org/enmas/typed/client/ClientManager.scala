package org.enmas.typed.client

import org.enmas.pomdp._, org.enmas.typed.messaging._,
       org.enmas.util.FileUtils._, org.enmas.util.voodoo.ClassLoaderUtils._,
       scala.collection.immutable._, java.io.File,
       //akka.actor._, akka.actor.Actor._, akka.dispatch._, akka.pattern.ask,
       akka.dispatch._, takka.actor._, takka.pattern._,
       akka.util.Timeout, scala.concurrent.duration._,
       com.typesafe.config.ConfigFactory, scala.concurrent.{ ExecutionContext, Promise }, scala.util.{Success, Failure}
import ExecutionContext.Implicits.global

class ClientManager extends TypedActor[ClientManagerMessage] with Provisionable {
  import ClientManager._, typedContext._

  private var sessions = List[ActiveSession]()
  private var POMDPs = Set[LocallyAvailablePOMDP]()
  loadUserPOMDPs

  private def loadUserPOMDPs { if (pomdpDir.isDirectory) {
    for (f  <- pomdpDir.listFiles) if (f.getName.endsWith(".jar")) {
      POMDPs ++= loadPOMDPsFromFile(f)
    }
  }}

  private def loadPOMDPsFromFile(jar: File): List[LocallyAvailablePOMDP] = {
    findSubclasses[POMDP](jar).filterNot { _.getName contains "$"} map {
      clazz  =>
        LocallyAvailablePOMDP(clazz.newInstance, jar)
    }
  }

  private def scanHost(address: String, replyTo: ActorRef[ClientManagerMessage]) {
    val host = actorFor[ServerManagerMessage](
      "akka://enmasServer@"+address+":"+serverPort+"/user/serverManager"
    )
    host ! RequestProvisions
    (host ? Discovery) onSuccess {
      case reply: DiscoveryReply  => replyTo ! reply
    }
  }

  private def createServer(address: String, pomdpClassName: String) {
    POMDPs.find(_.pomdp.getClass.getName == pomdpClassName) match {
      case Some(availablePOMDP)  => {
        readFile(availablePOMDP.jar) match {
          case Some(fileData)  => {
            val host = actorFor[ServerManagerMessage](
              "akka://enmasServer@"+address+":"+serverPort+"/user/serverManager"
            )
//            println("LALA "+Provision(fileData))
            host ! Provision(fileData)
//            println("WAWA "+CreateServerFor(pomdpClassName))            
            host ! CreateServerFor(pomdpClassName)
          }
          case None  => ()
        }
      }
      case None  => ()
    }
  }

  private def createSession(server: ServerSpec) {
    val replyTo = sender
    POMDPs.find( _.pomdp.getClass.getName == server.pomdpClassName) match {
      case Some(availablePOMDP)  => {
        val sessionRef = actorOf[ClientManagerMessage](Props(new Session(server.ref, availablePOMDP.pomdp)))
        watch(sessionRef) // subscribe to Terminated(sessionRef)
        (sessionRef ? Init) onComplete {
          case Success(e: Either[_,_])  => e match {
            case Left(obj)  => obj match { case id: Int  => {
              sessions = (ActiveSession(sessionRef, id, server) :: sessions)
              replyTo ! true
            }}
            case _ => replyTo ! false  
          }
            case Failure(_)  => replyTo ! false }
      }
      case None  => replyTo ! false
    }
  }

  
  def typedReceive = {

    case LoadPOMDPsFromFile(f)  => {
      POMDPs ++= loadPOMDPsFromFile(f) 
    }

    case GetLocalPOMDPs  => {
      sender ! POMDPList(POMDPs.toList map { _.pomdp }) 
    }

    case Provision(fileData: FileData)  => {
      val jarOption = provision[POMDP](fileData)
      jarOption map { jar  => { POMDPs ++= loadPOMDPsFromFile(jar) }}
    }

    case ScanHost(serverHost)  => {
     //scanHost(serverHost, sender) //TODO:  do something
     val senderRef = new ActorRef[ClientManagerMessage]{
       val untypedRef = sender
     }
     scanHost(serverHost, senderRef)
    }

    case CreateServer(serverHost, pomdpClassName)  => {
//      println("received message: create server "+serverHost)
      createServer(serverHost, pomdpClassName)      
    }


//    case e: Error  => e.cause.printStackTrace

    case m: CreateSession  => createSession(m.server)

    case GetSessions  => sender ! ActiveSessionList(sessions)

//    case error: Throwable  => { println(
//      "Error received from [%s]:\n%s".format(sender, error.getMessage)
//    )}

    case m  => println("unhandled message + "+ m) ; () // ignore unhandled messages
  }
  
  override def possiblyHarmfulHandler:akka.actor.PossiblyHarmful => Unit = {
    case akka.actor.Terminated(deceasedActor)  => {
      println("Received notice of some dead session")
      sessions.find(_.ref == deceasedActor) match { case Some(deadSession)  => {
          sessions = sessions filterNot { _ == deadSession }
          unwatch(deadSession.ref)
        }
        case None  => ()
      }
    }
  }
}

object ClientManager extends App {
  import org.enmas.typed.client.gui._, org.enmas.typed.client.http._, java.io.File

  // CM specific messages 
  sealed case class ScanHost(serverHost: String) extends ClientManagerMessage
  sealed case class CreateSession(server: ServerSpec) extends ClientManagerMessage
  sealed case class LoadPOMDPsFromFile(file: File) extends ClientManagerMessage
  case object GetSessions extends ClientManagerMessage
  case object GetLocalPOMDPs extends ClientManagerMessage
  sealed case class CreateServer(serverHost: String, pomdpClassName: String) extends ClientManagerMessage

  // tuple representing an active session
  sealed case class ActiveSession(ref: ActorRef[ClientManagerMessage], id: Int, server: ServerSpec)

  // tuple representing a POMDP and the JAR file containing its bytecode
  sealed case class LocallyAvailablePOMDP(pomdp: POMDP, jar: File) {
    override def equals(obj: Any): Boolean = obj match {
      case other: LocallyAvailablePOMDP  => pomdp.name == other.pomdp.name
      case _  => false
    }
  }

  sealed case class POMDPList(pomdps: List[POMDP])
  sealed case class ActiveSessionList(sessions: List[ActiveSession])

  // search locations for user-supplied code.
  val pomdpDir = new File("user/pomdp")
  val agentDir = new File("user/agent")
  val iterationSubscriberDir = new File("user/iterationSubscriber")

  implicit val timeout: Timeout = Timeout(3 seconds)
  val system = ActorSystem("enmasClient", ConfigFactory.load.getConfig("enmasClient"))
  val serverPort = 36627 // ENMAS
  val manager = system.actorOf(Props[ClientManagerMessage,ClientManager], "clientManager")
  val gui = new ClientGUI(manager)// TODO: refine this
  val net = new NetInterface(manager)
}