package org.enmas.client

import org.enmas.pomdp._, org.enmas.messaging._,
       org.enmas.client.gui._,
       scala.collection.immutable._,
       akka.actor._, akka.actor.Actor._, akka.dispatch._, akka.pattern.ask,
       scala.concurrent.Await,
       akka.util.Timeout, scala.concurrent.duration._, scala.concurrent.{ ExecutionContext, Promise }, scala.util.{Success, Failure}
import ExecutionContext.Implicits.global

class Session(server: ActorRef, pomdp: POMDP) extends Actor {
  import ClientManager._, Session._, context._

  watch(server) // subscribe to Terminated(server.ref)
  self ! Ping   // start the server polling loop
  private var uniqueID = -1
  private var agents = Map[Int, (AgentType, ActorRef)]()
  private var clients = Map[Int, ActorRef]()
  private val gui = new SessionGUI(self, pomdp)

  /** Returns true iff registering this clientManager session
    * with the specified server succeeds.
    *
    * Reasons that lead to registration failure are varied:
    * server is unreachable, server is not running, request denied,
    * or some other exceptional condition.
    */
  private def registerSelf(): Either[Int, Boolean] = {
    var result: Either[Int, Boolean] = Right(false)
    try { Await.result(server ? RegisterHost(self), timeout.duration) match {
      case c: ConfirmHostRegistration  ⇒ {
        uniqueID = c.id
        result = Left(uniqueID)
      }
    }}
    catch { case _:Throwable  ⇒ () }
    result
  }

  /** Replies to the sender with a ConfirmAgentRegistration message
    * iff the server confirms the agent registration
    * and creating the agent succeeds, or false otherwise.
    *
    * Upon successful registration, the agent is started and forwarded
    * the ConfirmAgentRegistration from the server.  The agent uses that
    * information for initialization and then become()s its user-defined
    * policy function.
    */
  private def registerAgent(
    agentType: AgentType,
    clazz: java.lang.Class[_ <: Agent]
  ) {
    val replyTo = sender
    (server ? RegisterAgent(uniqueID, agentType)) onComplete {
      case Success(confirmation: ConfirmAgentRegistration)  ⇒ {
        val agent = actorOf(Props(clazz.newInstance repliesTo self))
        watch(agent)
        agents += (confirmation.agentNumber  → (agentType, agent))
        agent forward confirmation
        replyTo ! confirmation
      }
      case Failure(_)  ⇒ replyTo ! false }
  }

  private def registerClient(clazz: java.lang.Class[_ <: IterationClient]) {
    def nextClientId = clients.foldLeft(0){ _ max _._1 } + 1
    try {
      val client = actorOf(Props(clazz.newInstance))
      // subscribe to Terminated(client)
      watch(client)
      // add the new client to the set of active clients
      val clientId = nextClientId
      clients += clientId  → client      
      // subscribe to POMDPIterations from the Server on this client's behalf
      server ! Subscribe
      sender ! ConfirmClientRegistration(clientId, clazz.getName)
    }
    catch { case _:Throwable  ⇒ sender ! false }
  }

  def receive = {

    case Ping  ⇒ {
      def doPoll {
        try { Thread.sleep(1000) }
        catch { case _:Throwable  ⇒ () }
        finally { self ! Ping }
      }
      (server ? Ping) onComplete {
        case Success(_)  ⇒ 
        gui.StatusBar.connected
        doPoll
        case Failure(_)  ⇒ {
        gui.StatusBar.noResponse
        doPoll
      }}
    }

    case 'Init  ⇒ sender ! registerSelf

    case m: LaunchAgent  ⇒ registerAgent(m.agentType, m.clazz)
    
    case m: LaunchClient  ⇒ registerClient(m.clazz)

    case iteration: POMDPIteration  ⇒ { clients map { _._2 ! iteration }}

    case MessageBundle(content)  ⇒ {
      if (sender == server) content map {
        c  ⇒ agents.find(_._1 == c.agentNumber) map { a  ⇒ a._2._2 ! c }}
    }

    case t: TakeAction  ⇒ {
      if (! (agents contains t.agentNumber)) sender ! PoisonPill
      agents.get(t.agentNumber) map { tuple  ⇒
        if (sender == tuple._2) server forward t else sender ! PoisonPill }
    }

    case Terminated(deceasedActor)  ⇒ {
      if (deceasedActor == server) { // the server died
        agents map { a  ⇒ { unwatch(a._2._2); stop(a._2._2) }}
        clients map { c  ⇒ { unwatch(c._2); stop(c._2) }}
        stop(self)
      }
      else {
        agents.find(_._2._2 == deceasedActor) match {
          case Some(deadAgent)  ⇒ { // one of this session's agents died
            server ! AgentDied(deadAgent._1)
            agents = agents filterNot { _ == deadAgent }
          }
          case None  ⇒ ()
        }
        clients find (_._2 == deceasedActor) match {
          case Some(deadClient)  ⇒ { // one of this session's clients died
            clients = clients filterNot { _ == deadClient }
            if (clients.isEmpty) server ! Unsubscribe
          }
          case None  ⇒ ()
        }
      }
    }

    case AgentDied(id)  ⇒ {
      // not sure if this alert is beneficial...
      println("An agent on another host (number "+id+") has died.")
    }

    case KillAgent(number)  ⇒
      agents filter { _._1 == number } map { _._2._2 ! Kill }
 
    case KillClient(number)  ⇒
      clients filter { _._1 == number } map { _._2 ! Kill }

    case _  ⇒ () // ignore unhandled messages
  }
}

object Session {
  sealed case class LaunchAgent(agentType: AgentType, clazz: java.lang.Class[_ <: Agent])
  sealed case class LaunchClient(clazz: java.lang.Class[_ <: IterationClient])
  sealed case class KillAgent(number: Int)
  sealed case class KillClient(number: Int)
}