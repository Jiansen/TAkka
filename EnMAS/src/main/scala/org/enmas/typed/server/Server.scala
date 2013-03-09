package org.enmas.typed.server

import org.enmas.pomdp._, org.enmas.typed.messaging._, org.enmas.util.FileUtils._,
       //akka.actor._, akka.actor.Actor._, 
       takka.actor._, 
       scala.util._, scala.collection.immutable._

class Server(pomdp: POMDP) extends TypedActor[ServerMessage] {
  private var state = pomdp.initialState
  private var iterationOrdinality = 0L
  private var sessions = Set[SessionSpec]()
  private var iterationSubscribers = Set[SessionSpec]()
  private var agents = Set[AgentSpec]()
  private var messageQueue = Map[SessionSpec, List[AgentMessage]]()
  private var pendingActions = List[AgentAction]()

  private var lastAgentId = 0
  private def nextAgentId: Int = { lastAgentId += 1; lastAgentId }

  private var lastSessionId = 0
  private def nextSessionId: Int = { lastSessionId += 1; lastSessionId }

  /** Creates a new SessionSpec object for the host and stores it in
    * the local list of sessions.  The SessionSpec contains a
    * unique id for this host.
    */
  private def registerHost(ref: ActorRef[ClientManagerMessage]): ClientManagerMessage = {//refined type
    val id = nextSessionId
    typedContext watch ref
    sessions += SessionSpec(id, ref)
    ConfirmHostRegistration(id)
  }

  /** Creates a new AgentSpec for the new Agent.  Replies with a
    * ConfirmAgentRegistration message if the POMDP accomodates the new
    * Agent, and with a DenyAgentRegistration message otherwise.
    */
  private def registerAgent(sessionID: Int, agentType: AgentType):SessionGUIMessage = {
    val a = AgentSpec(sessionID, nextAgentId, agentType)
    var newAgentSet = agents + a
    if (pomdp accomodatesAgents { newAgentSet.toList map {_.agentType} }) {
      agents = newAgentSet
      self ! TakeAction(a.agentNumber, NO_ACTION)
      ConfirmAgentRegistration(a.agentNumber, a.agentType, pomdp.actionsFunction(agentType))
    }
    else DenyAgentRegistration
  }

  /** Places a new AgentAction into the local collection of pending actions,
    * unless it already contains an action for the supplied agent number.
    *
    * Furthermore, if the current agents set satisfies the POMDP and all
    * agents have a pending action, iterates the simulation by calling the
    * iterate method.
    */
  private def takeAction(agentNumber: Int, action: Action) {
    if ((pendingActions filter { _.agentNumber == agentNumber }).isEmpty) {
      getAgent(agentNumber) map {
        a  ⇒ pendingActions ::= AgentAction(a.agentNumber, a.agentType, action)
      }
      if (
        pomdp.isSatisfiedByAgents(agents.toList map {_.agentType}) &&
        pendingActions.length == agents.size
      ) {
        state = iterate(state, pendingActions)
      }
    }
  }

  /** Iterates the simulation and returns the resulting next state.
    *
    * 1) Passes the supplied state and actions to the POMDP transitionFunction
    * 2) Uses the resulting probability distribution to select the next state
    * 3) Dispatches UpdateAgent messages
    */
  private def iterate(state: State, actions: JointAction): State = {
    try {
      val statePrime = pomdp.transitionFunction(state, actions) match {
        case Left(state)  ⇒ state
        case Right(distribution)  ⇒ selectState(distribution)
      }
      val reward = pomdp.rewardFunction(state, actions, statePrime)
      val observation = pomdp.observationFunction(state, actions, statePrime)
      var observations = Set[(AgentSpec, Observation)]()
      var rewards = Set[(AgentSpec, Float)]()

      sessions map { cm  ⇒ {
        val theseAgents = agents.toList.filter(_.sessionID == cm.id)
        for (a  ← theseAgents) {
          observations += (a  → observation(a.agentNumber, a.agentType))
          rewards += (a  → reward(a.agentType))
        }
        messageQueue += cm  → { for (a  ← theseAgents)
          yield UpdateAgent(
            a.agentNumber,
            observation(a.agentNumber, a.agentType),
            reward(a.agentType))}
      }}

      pendingActions = pendingActions take 0 // clears pending actions
      dispatchMessages // sends bundled agent updates
      // send iteration to POMDPIteration subscribers
      iterationSubscribers map { 
        _.ref ! POMDPIteration(iterationOrdinality, observations, rewards, actions, state)
      }
      iterationOrdinality += 1;
      statePrime
    }
    catch {
      case t: Throwable  ⇒ {
        t.printStackTrace
//        sessions map { cm  ⇒ cm.ref ! t } // TODO: check the type
        sessions map { cm  ⇒ cm.ref ! ClientManagerError(t) }        
      }
      state
    }
  }

  /** Returns one State according to a normalized probability distribution
    * represented as a List of (State, Int) tuples.
    */
  private def selectState(all: List[(State, Int)]) = {
    def stateAt(possible: List[(State, Int)], scalar: Int): State =
      if (scalar <= 0 || possible.tail == Nil)
        possible.head._1
      else stateAt(possible.tail, scalar - possible.head._2)

    val possible = all filter { _._2 > 0 }
    if (possible.size > 0) {
      val totalWeight = possible.foldLeft(0)((a, b)  ⇒ a + b._2)
      val randomScalar = (new Random) nextInt totalWeight
      stateAt(possible, randomScalar)
    }
    else state
  }

  /** Sends a MessageBundle object to each Session in the list of hosts.
    * The MessageBundle objects are culled from the outbound message queue.
    * This method also resets the outbound message queue.
    */
  private def dispatchMessages: Unit = {
    sessions map { cm  ⇒ { messageQueue.get(cm) map { cm.ref ! MessageBundle(_) }}}
    messageQueue = messageQueue.empty
  }

  /** Returns either Some[AgentSpec] containing an AgentSpec with the
    * supplied agentNumber, or None if no such object exists in the local
    * list of connected agents.
    */
  private def getAgent(agentNumber: Int) = agents.find( _.agentNumber == agentNumber )

  /** Returns either Some[Session] containing a Session with the
    * supplied id, or None if no such object exists in the local
    * list of active sessions.
    */
  private def getSession(id: Int) = sessions.find( _.id == id )

  /** Handles RegisterHost, RegisterAgent, and TakeAction messages by delegating to
    * the appropriate handler method.
    */
  def typedReceive = {

    case Ping  ⇒ sender ! Pong

    case reg: RegisterHost  ⇒ sender ! registerHost(reg.ref)

    case reg: RegisterAgent  ⇒ getSession(reg.sessionID) map {
      session  ⇒ sender ! registerAgent(session.id, reg.agentType)
    }

    case TakeAction(agentNumber, action)  ⇒ takeAction(agentNumber, action)

    case Subscribe  ⇒ sessions.find(_.ref.untypedRef == sender) match {
      case Some(subscriber)  ⇒ 
//        println("Server: subscriber "+subscriber)
        iterationSubscribers += subscriber
      case None  ⇒ 
//        println("Server: message from "+sender)
        ()
    }

    case Unsubscribe  ⇒ {
      iterationSubscribers = iterationSubscribers filterNot { _.ref == sender }
    }

    case AgentDied(id)  ⇒ {
      agents = agents filterNot { _.agentNumber == id }
      pendingActions = pendingActions filterNot { _.agentNumber == id }
      sessions filterNot { _.ref == sender } map { _.ref ! AgentDied(id) }
      println("An agent died!  [%s] agent(s) left" format agents.size)
    }

    case _  ⇒ ()
  }
  
  override def possiblyHarmfulHandler:akka.actor.PossiblyHarmful => Unit = {
    case akka.actor.Terminated(deceasedActor)  ⇒ {
      sessions.find(_.ref == deceasedActor) match { case Some(dead)  ⇒ {
          sessions find { _ == dead } match {
            case Some(deadSession)  ⇒ 
              agents = agents filterNot { _.sessionID == deadSession.id }
            case None  ⇒ ()
          }
          sessions = sessions filterNot { _ == dead }
          iterationSubscribers = iterationSubscribers filterNot { _ == dead }
          println("A session died! [%s] session(s) left" format sessions.size)
        }
        case None  ⇒ ()
      }
    }
    case _  ⇒ ()
  }
}