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
/**
 * Copied and modified from akka.actor.FSM.scala by Typesafe Inc. <http://www.typesafe.com>
 */
package takka.actor

import akka.util._
import scala.concurrent.duration.{FiniteDuration, Duration}
import scala.collection.mutable
import akka.event.Logging
import akka.routing.{ Deafen, Listen, Listeners }
import language.implicitConversions

object FSM {

  object NullFunction extends PartialFunction[Any, Nothing] {
    def isDefinedAt(o: Any) = false
    def apply(o: Any) = sys.error("undefined")
  }

  case class CurrentState[S, E](fsmRef: ActorRef[E], state: S)
  case class Transition[S, E](fsmRef: ActorRef[E], from: S, to: S)
  case class SubscribeTransitionCallBack[E](actorRef: ActorRef[E])
  case class UnsubscribeTransitionCallBack[E](actorRef: ActorRef[E])

  sealed trait Reason
  case object Normal extends Reason
  case object Shutdown extends Reason
  case class Failure(cause: Any) extends Reason

  case class TimeoutMarker(generation: Long)

  private[takka] case class Timer[E](name: String, msg: E, repeat: Boolean, generation: Int)(context: ActorContext[_]) {
    private var ref: Option[akka.actor.Cancellable] = _
    private val scheduler = context.system.system.scheduler
    private implicit val executionContext = context.untypedContext.dispatcher
    
    def schedule(actor: ActorRef[E], timeout: FiniteDuration): Unit =
      ref = Some(
        if (repeat) scheduler.schedule(timeout, timeout, actor.untypedRef, this)
        else scheduler.scheduleOnce(timeout, actor.untypedRef, this))

    def cancel(): Unit = if (ref.isDefined) {
      ref.get.cancel()
      ref = None
    }
  }

  /**
   * This extractor is just convenience for matching a (S, S) pair, including a
   * reminder what the new state is.
   */
  object -> {
    def unapply[S](in: (S, S)) = Some(in)
  }

  case class LogEntry[S, D, E](stateName: S, stateData: D, event: E)

  case class State[S, D](stateName: S, stateData: D, timeout: Option[FiniteDuration] = None, stopReason: Option[Reason] = None, replies: List[(ActorRef[_], _)] = Nil) {

    /**
     * Modify state transition descriptor to include a state timeout for the
     * next state. This timeout overrides any default timeout set for the next
     * state.
     */
    def forMax(timeout: Duration): State[S, D] = timeout match {
      case f: FiniteDuration => copy(timeout = Some(f))
      case _                 => copy(timeout = None)
    }

    /**
     * Send reply to sender of the current message, if available.
     *
     * @return this state transition descriptor
     */
    // Changes: each reply value matches the actor it replying to
    def replying[M](to:ActorRef[M], replyValue: M): State[S, D] = {
      copy(replies = (to, replyValue) :: replies)
    }

    /**
     * Modify state transition descriptor with new state data. The data will be
     * set when transitioning to the new state.
     */
    def using(nextStateDate: D): State[S, D] = {
      copy(stateData = nextStateDate)
    }

    private[actor] def withStopReason(reason: Reason): State[S, D] = {
      copy(stopReason = Some(reason))
    }
  }

}

/**
 * Finite State Machine actor trait. Use as follows:
 *
 * <pre>
 *   object A {
 *     trait State
 *     case class One extends State
 *     case class Two extends State
 *
 *     case class Data(i : Int)
 *   }
 *
 *   class A extends Actor with FSM[A.State, A.Data] {
 *     import A._
 *
 *     startWith(One, Data(42))
 *     when(One) {
 *         case Event(SomeMsg, Data(x)) => ...
 *         case Ev(SomeMsg) => ... // convenience when data not needed
 *     }
 *     when(Two, stateTimeout = 5 seconds) { ... }
 *     initialize
 *   }
 * </pre>
 *
 * Within the partial function the following values are returned for effecting
 * state transitions:
 *
 *  - <code>stay</code> for staying in the same state
 *  - <code>stay using Data(...)</code> for staying in the same state, but with
 *    different data
 *  - <code>stay forMax 5.millis</code> for staying with a state timeout; can be
 *    combined with <code>using</code>
 *  - <code>goto(...)</code> for changing into a different state; also supports
 *    <code>using</code> and <code>forMax</code>
 *  - <code>stop</code> for terminating this FSM actor
 *
 * Each of the above also supports the method <code>replying(AnyRef)</code> for
 * sending a reply before changing state.
 *
 * While changing state, custom handlers may be invoked which are registered
 * using <code>onTransition</code>. This is meant to enable concentrating
 * different concerns in different places; you may choose to use
 * <code>when</code> for describing the properties of a state, including of
 * course initiating transitions, but you can describe the transitions using
 * <code>onTransition</code> to avoid having to duplicate that code among
 * multiple paths which lead to a transition:
 *
 * <pre>
 * onTransition {
 *   case Active -&gt; _ =&gt; cancelTimer("activeTimer")
 * }
 * </pre>
 *
 * Multiple such blocks are supported and all of them will be called, not only
 * the first matching one.
 *
 * Another feature is that other actors may subscribe for transition events by
 * sending a <code>SubscribeTransitionCallback</code> message to this actor;
 * use <code>UnsubscribeTransitionCallback</code> before stopping the other
 * actor.
 *
 * State timeouts set an upper bound to the time which may pass before another
 * message is received in the current state. If no external message is
 * available, then upon expiry of the timeout a StateTimeout message is sent.
 * Note that this message will only be received in the state for which the
 * timeout was set and that any message received will cancel the timeout
 * (possibly to be started again by the next transition).
 *
 * Another feature is the ability to install and cancel single-shot as well as
 * repeated timers which arrange for the sending of a user-specified message:
 *
 * <pre>
 *   setTimer("tock", TockMsg, 1 second, true) // repeating
 *   setTimer("lifetime", TerminateMsg, 1 hour, false) // single-shot
 *   cancelTimer("tock")
 *   timerActive_? ("tock")
 * </pre>
 */
trait FSM[S, D, E] extends Listeners {
  
  this: TypedActor[E] =>

  import FSM._

  type State = FSM.State[S, D]
  type StateFunction = scala.PartialFunction[FSMTransaction, State]
  
  type Timeout = Option[FiniteDuration]
  type TransitionHandler = PartialFunction[(S, S), Unit]

  // “import” so that it is visible without an import
  val -> = FSM.->
  //val StateTimeout = FSM.StateTimeout
  
  
  val log = Logging(context.system, this)

  /**
   * ****************************************
   *                 DSL
   * ****************************************
   */

  /**
   * Insert a new StateFunction at the end of the processing chain for the
   * given state. If the stateTimeout parameter is set, entering this state
   * without a differing explicit timeout setting will trigger a StateTimeout
   * event; the same is true when using #stay.
   *
   * @param stateName designator for the state
   * @param stateTimeout default state timeout for this state
   * @param stateFunction partial function describing response to input
   */
  protected final def when(stateName: S, stateTimeout: FiniteDuration = null)(stateFunction: StateFunction): Unit =
    register(stateName, stateFunction, Option(stateTimeout))

  @deprecated("use the more import-friendly variant taking a Duration", "2.0")
  protected final def when(stateName: S, stateTimeout: Timeout)(stateFunction: StateFunction): Unit =
    register(stateName, stateFunction, stateTimeout)

  /**
   * Set initial state. Call this method from the constructor before the #initialize method.
   *
   * @param stateName initial state designator
   * @param stateData initial state data
   * @param timeout state timeout for the initial state, overriding the default timeout for that state
   */
  protected final def startWith(stateName: S,
                                stateData: D,
                                timeout: Timeout = None): Unit =
    currentState = FSM.State(stateName, stateData, timeout)

  /**
   * Produce transition to other state. Return this from a state function in
   * order to effect the transition.
   *
   * @param nextStateName state designator for the next state
   * @return state transition descriptor
   */
  protected final def goto(nextStateName: S): State = FSM.State(nextStateName, currentState.stateData)

  /**
   * Produce "empty" transition descriptor. Return this from a state function
   * when no state change is to be effected.
   *
   * @return descriptor for staying in current state
   */
  protected final def stay(): State = goto(currentState.stateName) // cannot directly use currentState because of the timeout field

  /**
   * Produce change descriptor to stop this FSM actor with reason "Normal".
   */
  protected final def stop(): State = stop(Normal)

  /**
   * Produce change descriptor to stop this FSM actor including specified reason.
   */
  protected final def stop(reason: Reason): State = stop(reason, currentState.stateData)

  /**
   * Produce change descriptor to stop this FSM actor including specified reason.
   */
  protected final def stop(reason: Reason, stateData: D): State = stay using stateData withStopReason (reason)

  /**
   * Schedule named timer to deliver message after given delay, possibly repeating.
   * @param name identifier to be used with cancelTimer()
   * @param msg message to be delivered
   * @param timeout delay of first message delivery and between subsequent messages
   * @param repeat send once if false, scheduleAtFixedRate if true
   * @return current state descriptor
   */
  protected[actor] def setTimer(name: String, msg: E, timeout: FiniteDuration, repeat: Boolean): State = {
    if (timers contains name) {
      timers(name).cancel
    }

    val timer = Timer[E](name, msg, repeat, timerGen.next)(typedContext)
    timer.schedule(typedSelf, timeout)
    timers(name) = timer
    stay
  }

  /**
   * Cancel named timer, ensuring that the message is not subsequently delivered (no race).
   * @param name of the timer to cancel
   */
  protected[actor] def cancelTimer(name: String): Unit =
    if (timers contains name) {
      timers(name).cancel
      timers -= name
    }

  /**
   * Inquire whether the named timer is still active. Returns true unless the
   * timer does not exist, has previously been canceled or if it was a
   * single-shot timer whose message was already received.
   */
  protected[actor] final def timerActive_?(name: String) = timers contains name

  /**
   * Set state timeout explicitly. This method can safely be used from within a
   * state handler.
   */
  protected final def setStateTimeout(state: S, timeout: Timeout): Unit = stateTimeouts(state) = timeout

  /**
   * Set handler which is called upon each state transition, i.e. not when
   * staying in the same state. This may use the pair extractor defined in the
   * FSM companion object like so:
   *
   * <pre>
   * onTransition {
   *   case Old -&gt; New =&gt; doSomething
   * }
   * </pre>
   *
   * It is also possible to supply a 2-ary function object:
   *
   * <pre>
   * onTransition(handler _)
   *
   * private def handler(from: S, to: S) { ... }
   * </pre>
   *
   * The underscore is unfortunately necessary to enable the nicer syntax shown
   * above (it uses the implicit conversion total2pf under the hood).
   *
   * <b>Multiple handlers may be installed, and every one of them will be
   * called, not only the first one matching.</b>
   */
  protected final def onTransition(transitionHandler: TransitionHandler): Unit = transitionEvent :+= transitionHandler

  /**
   * Convenience wrapper for using a total function instead of a partial
   * function literal. To be used with onTransition.
   */
  implicit protected final def total2pf(transitionHandler: (S, S) => Unit) =
    new TransitionHandler {
      def isDefinedAt(in: (S, S)) = true
      def apply(in: (S, S)) { transitionHandler(in._1, in._2) }
    }

  /**
   * Set handler which is called upon termination of this FSM actor.
   */
  protected final def onTermination(terminationHandler: PartialFunction[StopEvent[S, D], Unit]): Unit =
    terminateEvent = terminationHandler

  /**
   * Set handler which is called upon reception of unhandled messages.
   */
  protected final def whenUnhandled(stateFunction: StateFunction): Unit =
    handleEvent = stateFunction orElse handleEventDefault

  /**
   * Verify existence of initial state and setup timers. This should be the
   * last call within the constructor.
   */
  protected final def initialize: Unit = makeTransition(currentState)

  /**
   * Return current state name (i.e. object of type S)
   */
  protected[actor] def stateName: S = currentState.stateName

  /**
   * Return current state data (i.e. object of type D)
   */
  protected[actor] def stateData: D = currentState.stateData

  /**
   * Return next state data (available in onTransition handlers)
   */
  protected[actor] def nextStateData = nextState.stateData

  /*
   * ****************************************************************
   *                PRIVATE IMPLEMENTATION DETAILS
   * ****************************************************************
   */

  /*
   * FSM State data and current timeout handling
   */
  protected var currentState: State = _
  private var timeoutFuture: Option[akka.actor.Cancellable] = None
  private var nextState: State = _
  private var generation: Long = 0L

  /*
   * Timer handling
   */
  private val timers = mutable.Map[String, Timer[E]]()
  private val timerGen = Iterator from 0

  /*
   * State definitions
   */
  private val stateFunctions = mutable.Map[S, StateFunction]()
  private val stateTimeouts = mutable.Map[S, Timeout]()

  private def register(name: S, function: StateFunction, timeout: Timeout): Unit = {
    if (stateFunctions contains name) {
      stateFunctions(name) = stateFunctions(name) orElse function
      stateTimeouts(name) = timeout orElse stateTimeouts(name)
    } else {
      stateFunctions(name) = function
      stateTimeouts(name) = timeout
    }
  }

  /*
   * unhandled event handler
   */
  private val handleEventDefault: StateFunction = {
    case Event(value, stateData) =>
      log.warning("unhandled event " + value + " in state " + stateName)
      stay
    case StateTimeout(_) =>
      log.warning("unhandled StateTimeout in state " + stateName)
      stay
  }
  private var handleEvent: StateFunction = handleEventDefault

  /*
   * termination handling
   */
  private var terminateEvent: PartialFunction[StopEvent[S, D], Unit] = NullFunction

  /*
   * transition handling
   */
  private var transitionEvent: List[TransitionHandler] = Nil
  private def handleTransition(prev: S, next: S) {
    val tuple = (prev, next)
    for (te <- transitionEvent) { if (te.isDefinedAt(tuple)) te(tuple) }
  }

  /*
   * *******************************************
   *       Main actor receive() method
   * *******************************************
   */
  //not important any more because receive is well defined.
  override final protected def typedReceive:PartialFunction[E, Unit] = {
    case _ =>
  }
  
  override final def receive: Receive = {
    case TimeoutMarker(gen) =>
      if (generation == gen) {
        processMsg(Right(StateTimeout), "state timeout") 
      }
    case t @ Timer(name, msg, repeat, gen) =>
      if ((timers contains name) && (timers(name).generation == gen)) {
        if (timeoutFuture.isDefined) {
          timeoutFuture.get.cancel()
          timeoutFuture = None
        }
        generation += 1
        if (!repeat) {
          timers -= name
        }
        processMsg(msg.asInstanceOf[Either[E, StateTimeout.type]], t)
      }
    case SubscribeTransitionCallBack(actorRef) =>
      // TODO use DeathWatch to clean up list
      listeners.add(actorRef.untypedRef)
      // send current state back as reference point
      actorRef.untypedRef ! CurrentState(typedSelf, currentState.stateName)
    case Listen(actorRef) =>
      // TODO use DeathWatch to clean up list
      listeners.add(actorRef)
      // send current state back as reference point
      actorRef ! CurrentState(typedSelf, currentState.stateName)
    case UnsubscribeTransitionCallBack(actorRef) =>
      listeners.remove(actorRef)
    case Deafen(actorRef) =>
      listeners.remove(actorRef)
    case StateTimeout => {
      if (timeoutFuture.isDefined) {
        timeoutFuture.get.cancel()
        timeoutFuture = None
      }
      generation += 1
      processMsg(Right(StateTimeout), sender)
    }
      
    case value:E => {
      if (timeoutFuture.isDefined) {
        timeoutFuture.get.cancel()
        timeoutFuture = None
      }
      generation += 1
      processMsg(Left(value), sender)
    }

  }

  private def processMsg(msg: Either[E, StateTimeout.type], source: AnyRef): Unit = {
    if (msg.isLeft){
      val event = Event(msg.left.get, currentState.stateData)
      processEvent(event, source)      
    }else{//is timeout
      processEvent(StateTimeout(currentState.stateData), source)
    }
  }

  private[actor] def processEvent(event: FSMTransaction, source: AnyRef): Unit = {
    val stateFunc = stateFunctions(currentState.stateName)
    val nextState = if (stateFunc isDefinedAt event) {
      stateFunc(event)
    } else {
      // handleEventDefault ensures that this is always defined
      handleEvent(event)
    }
    applyState(nextState)
  }

  private[actor] def applyState(nextState: State): Unit = {
    nextState.stopReason match {
      case None => makeTransition(nextState)
      case _ =>
        nextState.replies.reverse foreach { case (t,r) => t.untypedRef ! r } // type safety is guaranteed by API implementation
        terminate(nextState)
        typedContext.stop(typedSelf)
    }
  }

  private[actor] def makeTransition(nextState: State): Unit = {
    implicit val executionContext = context.dispatcher
    if (!stateFunctions.contains(nextState.stateName)) {
      terminate(stay withStopReason Failure("Next state %s does not exist".format(nextState.stateName)))
    } else {
      nextState.replies.reverse foreach { case (t,r) => t.untypedRef ! r } // type safety is guaranteed by API implementation
      if (currentState.stateName != nextState.stateName) {
        this.nextState = nextState
        handleTransition(currentState.stateName, nextState.stateName)
        gossip(Transition(typedSelf, currentState.stateName, nextState.stateName))
      }
      currentState = nextState
      val timeout = if (currentState.timeout.isDefined) currentState.timeout else stateTimeouts(currentState.stateName)
      if (timeout.isDefined) {
        val t = timeout.get
        if (t.isFinite && t.length >= 0) {
          timeoutFuture = Some(context.system.scheduler.scheduleOnce(t, self, TimeoutMarker(generation)))
        }
      }
    }
  }

  override def postStop(): Unit = { terminate(stay withStopReason Shutdown) }

  private def terminate(nextState: State): Unit = {
    if (!currentState.stopReason.isDefined) {
      val reason = nextState.stopReason.get
      reason match {
        case Failure(ex: Throwable) => log.error(ex, "terminating due to Failure")
        case Failure(msg: AnyRef)   => log.error(msg.toString)
        case _                      =>
      }
      val stopEvent = StopEvent(reason, currentState.stateName, currentState.stateData)
      if (terminateEvent.isDefinedAt(stopEvent))
        terminateEvent(stopEvent)
      currentState = nextState
    }
  }

  sealed trait FSMTransaction
  case class Event(event: E, stateData: D) extends FSMTransaction
  case class StateTimeout(stateData: D) extends FSMTransaction
  
  case class StopEvent[S, D](reason: Reason, currentState: S, stateData: D)
}

/**
 * Stackable trait for FSM which adds a rolling event log.
 *
 * @since 1.2
 */
trait LoggingFSM[S, D, E] extends FSM[S, D, E] { this: TypedActor[E] =>

  import FSM._

  def logDepth: Int = 0

  private val debugEvent = context.system.settings.FsmDebugEvent

  private val events = new Array[Event](logDepth)
  private val states = new Array[AnyRef](logDepth)
  private var pos = 0
  private var full = false

  private def advance() {
    val n = pos + 1
    if (n == logDepth) {
      full = true
      pos = 0
    } else {
      pos = n
    }
  }

  protected[actor] abstract override def setTimer(name: String, msg: E, timeout: FiniteDuration, repeat: Boolean): State = {
    if (debugEvent)
      log.debug("setting " + (if (repeat) "repeating " else "") + "timer '" + name + "'/" + timeout + ": " + msg)
    super.setTimer(name, msg, timeout, repeat)
  }

  protected[actor] abstract override def cancelTimer(name: String): Unit = {
    if (debugEvent)
      log.debug("canceling timer '" + name + "'")
    super.cancelTimer(name)
  }

  private[actor] abstract override def processEvent(event: FSMTransaction, source: AnyRef): Unit = {
    if (debugEvent) {
      val srcstr = source match {
        case s: String            => s
        case Timer(name, _, _, _) => "timer " + name
        case a: ActorRef[E]          => a.toString
        case _                    => "unknown"
      }
      log.debug("processing " + event + " from " + srcstr)
    }

    if (logDepth > 0) {
      states(pos) = stateName.asInstanceOf[AnyRef]
      if (event.isInstanceOf[Event]){
        events(pos) = event.asInstanceOf[Event]   
      }
      advance()
    }

    val oldState = stateName
    super.processEvent(event, source)
    val newState = stateName

    if (debugEvent && oldState != newState)
      log.debug("transition " + oldState + " -> " + newState)
  }

  /**
   * Retrieve current rolling log in oldest-first order. The log is filled with
   * each incoming event before processing by the user supplied state handler.
   */
  protected def getLog: IndexedSeq[LogEntry[S, D, E]] = {
    val log = events zip states filter (_._1 ne null) map (x => LogEntry(x._2.asInstanceOf[S], x._1.stateData, x._1.event))
    if (full) {
      IndexedSeq() ++ log.drop(pos) ++ log.take(pos)
    } else {
      IndexedSeq() ++ log
    }
  }

}
