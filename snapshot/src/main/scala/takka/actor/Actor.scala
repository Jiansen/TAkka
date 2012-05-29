package takka.actor

/**
 * Actor base trait <a href="http://en.wikipedia.org/wiki/Actor_model">http://en.wikipedia.org/wiki/Actor_model</a>
 * that takes a type parameter (M) denoting the type of the actor's initial message loop (M => Unit).
 *
 * An actor has a well-defined (non-cyclic) life-cycle.
 *  - ''RUNNING'' (created and started actor) - can receive messages
 *  - ''SHUTDOWN'' (when 'stop' or 'exit' is invoked) - can't do anything
 *
 * The Actor's own [[takka.actor.ActorRef]] is available as `typedSelf`,
 *  [[takka.actor.ActorContext]] as `typedContext`. The only abstract method is `typedReceive` which shall return the
 * initial behavior of the actor as a partial function (behavior can be changed
 * using `typedContext.become`).
 *
 * Following example is adapted from ***
 *
 * {{{
 * sealed trait Message
 * sealed trait Result
 * case class Request(j:Job, reply:Actor[Result]) extends Message
 * case object Shutdown extends Message
 * case class Dangerous(j:Job, sender:Worker) extends Message
 * case class OtherJob(j:Job, sender:Worker) extends Message
 * case class JobReply(result:Result, orig_s:Actor[Result]) extends Message
 * 
 * class ExampleActor extends Actor[Message] {
 *
 *   override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
 *     case _: ArithmeticException      => Resume
 *     case _: NullPointerException     => Restart
 *     case _: IllegalArgumentException => Stop
 *     case _: Exception                => Escalate
 *   }
 *
 *   def typedReceive = {
 *                                      // directly calculated reply
 *     case Request(j, reply)        => reply ! calculate(j)
 *
 *                                      // just to demonstrate how to stop yourself
 *     case Shutdown                 => typedContext.stop(typedSelf)
 *
 *                                      // error kernel with child replying directly to “customer”
 *     case Dangerous(j, sender)     => typedContext.actorOf(Props[ReplyToOriginWorker]).tell(PerformWork(j), sender)
 *
 *                                      // error kernel with reply going through us
 *     case OtherJob(j, sender)      => context.actorOf(Props[ReplyToMeWorker]) ! JobRequest(j, sender)
 *     case JobReply(result, orig_s) => orig_s ! result
 *   }
 * }
 * }}}
 *
 * The Dangerous and OtherJob cases demonstrate the essence of the error kernel design: spawn
 * one-off actors which terminate after doing their job, pass on `sender` to
 * allow direct reply if that is what makes sense, or round-trip the sender
 * as shown with the fictitious JobRequest/JobReply message pair.
 *
 * If you don’t like writing `typedContext` you can always `import typedContext._` to get
 * direct access to `actorOf`, `stop` etc. This is not default in order to keep
 * the name-space clean.
 */
//trait Actor[-Msg] extends akka.actor.Actor {
//trait Actor[Msg](implicit msgT:Manifest[Msg]) extends akka.actor.Actor {
//trait Actor[Msg:Manifest] extends akka.actor.Actor {
trait Actor[M] extends akka.actor.Actor{  
  /**
   * This defines the initial actor behavior, it must return a partial function
   * with the actor logic.
   */
  protected def typedReceive:PartialFunction[M, Unit]
  protected def receive = {
    case hmsg:akka.actor.PossiblyHarmful => possiblyHarmfulHandler(hmsg)    
//    case m:Msg if (typedReceive.isDefinedAt(m)) => typedReceive(m)
    case m:M => typedReceive(m)    
//    case akka.actor.ReceiveTimeout => receiveTimeout ()
    case x => throw new Exception("Message "+x+" has the wrong type.")
  }

   /**
   * Stores the context for this actor, including typedSelf
   */
  protected[actor] implicit val typedContext:ActorContext[M] = new ActorContext[M]{
     val untyped_context = context
     val props:Props[M] = Props(untyped_context.props)
  }
  
  /**
   * The 'typedSelf' field holds the ActorRef[M] for this actor,
   * which typically cannot be used outside the local machine.
   * <p/>
   * Can be used to send messages to itself:
   * <pre>
   * typedSelf ! message
   * </pre>
   */
  implicit final val typedSelf:ActorRef[M] = new ActorRef[M] {
    val untyped_ref = self
  } 
  
  /**
   * The 'typedRemoteSelf' field holds the ActorRef[M] for this actor,
   * which can be used across the network, if the ActorSystem where
   * this actor locate in listens to an active port.
   */
  final lazy val typedRemoteSelf:ActorRef[M] =  {
    val system = typedContext.system
    new ActorRef[M] {
      val localPathStr = self.path.toString()
      val sys_path = localPathStr.split("@")
      val remotePathStr = sys_path(0)+"@"+system.host+":"+system.port+sys_path(1)
//akka://RemoteCreation@129.215.91.195:2554/user/creationActor    
      val untyped_ref = context.actorFor(remotePathStr)
    } 
  } 
  // def receiveTimeout : () => Unit = () => {}
  
  /**
   * handler of untyped harmful features
   */
  //TODO: make it typed
  def possiblyHarmfulHandler:akka.actor.PossiblyHarmful => Unit = {
    case _ => 
      /*
case class Failed(cause: Throwable) extends AutoReceivedMessage with PossiblyHarmful
case object PoisonPill extends AutoReceivedMessage with PossiblyHarmful
case object Kill extends AutoReceivedMessage with PossiblyHarmful
case class Terminated(@BeanProperty actor: ActorRef) extends PossiblyHarmful
case object ReceiveTimeout extends PossiblyHarmful      
      */
  }
}