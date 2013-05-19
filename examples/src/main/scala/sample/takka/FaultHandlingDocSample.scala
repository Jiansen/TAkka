package sample.takka

import akka.actor.ActorLogging

import takka.actor._
import akka.actor.SupervisorStrategy._
import scala.concurrent.duration._
import akka.util.Timeout
import akka.event.LoggingReceive
import akka.pattern.ask
import com.typesafe.config.ConfigFactory
import language.postfixOps
 
/**
 * Runs the sample
 */
object FaultHandlingDocSample extends App {
  import Worker._
 
  val config = ConfigFactory.parseString("""
    akka.loglevel = DEBUG
    akka.actor.debug {
      receive = on
      lifecycle = on
    }
    """)
 
  val system = ActorSystem("FaultToleranceSample", config)
//  val worker = system.actorOf(Props[WorkerMessage, Worker], name = "worker")
//  val listener = system.actorOf(Props[ListenerMessage, Listener], name = "listener")
  // start the work and listen on progress
  // note that the listener is used as sender of the tell,
  // i.e. it will receive replies from the worker
//  worker ! Start(listener)
}
 
/**
 * Listens on progress from the worker and shuts down the system when enough
 * work has been done.
 */
class Listener extends TypedActor[ListenerMessage] with ActorLogging {
  import Worker._
  // If we don't get any progress within 15 seconds then the service is unavailable
  context.setReceiveTimeout(15 seconds)
 
  def typedReceive = {
    case Progress(percent) =>
      log.info("Current progress: {} %", percent)
      if (percent >= 100.0) {
        log.info("That's all, shutting down")
        context.system.shutdown()
      }
  }
  
  override def systemMessageHandler = {
    case ReceiveTimeout =>
      // No progress within 15 seconds, ServiceUnavailable
      log.error("Shutting down due to unavailable service")
      context.system.shutdown()
  }

}
 
sealed trait ListenerMessage
sealed trait WorkerMessage
object Worker {
  case class Start(listerner:ActorRef[ListenerMessage]) extends WorkerMessage
  case object Do extends WorkerMessage
  case class Progress(percent: Double) extends WorkerMessage with ListenerMessage
}
 
/**
 * Worker performs some work when it receives the `Start` message.
 * It will continuously notify the sender of the `Start` message
 * of current ``Progress``. The `Worker` supervise the `CounterService`.
 */
class Worker extends TypedActor[WorkerMessage] with ActorLogging {
  import Worker._
  import CounterService._
  implicit val askTimeout = Timeout(5 seconds)
  import context.dispatcher // Use this Actors' Dispatcher as ExecutionContext
  
  // Stop the CounterService child if it throws ServiceUnavailable
  override val supervisorStrategy = akka.actor.OneForOneStrategy() {
    case _: CounterService.ServiceUnavailable => akka.actor.SupervisorStrategy.Stop
  }
 
  // The sender of the initial Start message will continuously be notified about progress
  var progressListener: Option[ActorRef[ListenerMessage]] = None
  val counterService:ActorRef[CounterServiceMessage] = typedContext.actorOf(Props[CounterServiceMessage, CounterService], name = "counter")
  val totalCount = 51
 
  def typedReceive = {
    case Start(listerner) if progressListener.isEmpty =>
      progressListener = Some(listerner)
      context.system.scheduler.schedule(Duration.Zero, 1 second, self, Do)
 
    case Do =>
      counterService ! WorkerIncrement(1)
      counterService ! WorkerIncrement(1)
      counterService ! WorkerIncrement(1)
 
      // Send current progress to the initial sender
      /*
      counterService ? GetCurrentCount map {
        case CurrentCount(_, count) ��� Progress(100.0 * count / totalCount)
      } pipeTo progressListener.get
      */
      
      /*
      pattern.Patterns.ask(counterService, WorkerGetCurrentCount(typedSelf), 10000) map {
        case CurrentCount(_, count) ��� Progress(100.0 * count / totalCount)
      } pipeTo progressListener.get
      */
      
      counterService ! WorkerGetCurrentCount(typedSelf)
    case CurrentCount(_, count) =>
      progressListener.get ! Progress(100.0 * count / totalCount)
  }
}


sealed trait CounterServiceMessage
object CounterService {
  case class WorkerIncrement(n: Int) extends CounterServiceMessage
  case class WorkerGetCurrentCount(worker:ActorRef[WorkerMessage]) extends CounterServiceMessage
  case class CurrentCount(key: String, count: Long) extends WorkerMessage
  class ServiceUnavailable(msg: String) extends RuntimeException(msg)
 
  private case object Reconnect
}
 
/**
 * Adds the value received in `Increment` message to a persistent
 * counter. Replies with `CurrentCount` when it is asked for `CurrentCount`.
 * `CounterService` supervise `Storage` and `Counter`.
 */
class CounterService extends TypedActor[CounterServiceMessage] {
  import CounterService._
  import Counter._
  import Storage._
 
  // Restart the storage child when StorageException is thrown.
  // After 3 restarts within 5 seconds it will be stopped.
  override val supervisorStrategy = akka.actor.OneForOneStrategy(maxNrOfRetries = 3, withinTimeRange = 5 seconds) {
    case _: Storage.StorageException => Restart
  }
 
  val key = self.path.name
  var storage: Option[ActorRef[StorageMessage]] = None
  var counter: Option[ActorRef[CounterMessage]] = None
  var backlog = IndexedSeq.empty[CounterMessage]// recipiant is part of the message
  val MaxBacklog = 10000
 
  import context.dispatcher // Use this Actors' Dispatcher as ExecutionContext
  
  override def preStart() {
    initStorage()
  }
 
  /**
   * The child storage is restarted in case of failure, but after 3 restarts,
   * and still failing it will be stopped. Better to back-off than continuously
   * failing. When it has been stopped we will schedule a Reconnect after a delay.
   * Watch the child so we receive Terminated message when it has been terminated.
   */
  def initStorage() {
    storage = Some(typedContext.watch(typedContext.actorOf(Props[StorageMessage, Storage], name = "storage")))
    // Tell the counter, if any, to use the new storage
    counter foreach { _ ! UseStorage(storage) }
    // We need the initial value to be able to operate

    storage.get ! Get(key, typedSelf)
  }
 
  def typedReceive = LoggingReceive { 
    case Entry(k, v) if k == key && counter == None =>
      // Reply from Storage of the initial value, now we can create the Counter
      val c = typedContext.actorOf(Props[CounterMessage](new Counter(key, v)))
      counter = Some(c)
      // Tell the counter to use current storage
      c ! UseStorage(storage)
      // and send the buffered backlog to the counter
      
      // The type of msg is now refined because we know that c:ActorRef[CounterMessage]
      // for ((replyTo, msg) <- backlog) c.ref.tell(msg, sender = replyTo) //type refined
      for (msg <- backlog) { c ! msg }      
      backlog = IndexedSeq.empty
 
    case WorkerIncrement(n) => forwardOrPlaceInBacklog(CounterServiceIncrement(n)) // worker is discarded
 
    case WorkerGetCurrentCount(worker) => forwardOrPlaceInBacklog(CounterServiceGetCurrentCount(worker))
 
    case Reconnect =>
      // Re-establish storage after the scheduled delay
      initStorage()
//    case _ => 
  }
  /*
  override def possiblyHarmfulHandler = {
    case akka.actor.Terminated(actorRef) if actorRef == storage.get.untypedRef ���
      // After 3 restarts the storage child is stopped.
      // We receive Terminated because we watch the child, see initStorage.
      storage = None
      // Tell the counter that there is no storage for the moment
      counter foreach { _ ! UseStorage(None) }
      // Try to re-establish storage after while
      context.system.scheduler.scheduleOnce(10 seconds, self, Reconnect) //TODO: untyped feature
  }
  */
  def forwardOrPlaceInBacklog(msg: CounterMessage) {  //  TODO: refine message 
  //def forwardOrPlaceInBacklog(msg: CounterMessage, sender:ActorRef[CounterMessage]) {
    // We need the initial value from storage before we can start delegate to the counter.
    // Before that we place the messages in a backlog, to be sent to the counter when
    // it is initialized.
    counter match {
      case Some(c) => c ! msg
      case None =>
        if (backlog.size >= MaxBacklog)
          throw new ServiceUnavailable("CounterService not available, lack of initial value")
        backlog = backlog :+ msg
    }
  }
 
}
 
sealed trait CounterMessage
object Counter {
  case class UseStorage(storage: Option[ActorRef[StorageMessage]]) extends CounterMessage
  
  case class CounterServiceIncrement(n: Int) extends CounterMessage
  case class CounterServiceGetCurrentCount(worker:ActorRef[WorkerMessage]) extends CounterMessage
}
 
/**
 * The in memory count variable that will send current
 * value to the `Storage`, if there is any storage
 * available at the moment.
 */
class Counter(key: String, initialValue: Long) extends TypedActor[CounterMessage] {
  import Counter._
  import CounterService._
  import Storage._
 
  var count = initialValue
  var storage: Option[ActorRef[StorageMessage]] = None
 
  def typedReceive = {
    case UseStorage(s) =>
      storage = s
      storeCount()
 
    case CounterServiceIncrement(n) =>
      count += n
      storeCount()
 
    case CounterServiceGetCurrentCount(worker) =>
      worker ! CurrentCount(key, count)
 
  }
 
  def storeCount() {
    // Delegate dangerous work, to protect our valuable state.
    // We can continue without storage.
    storage foreach { _ ! Store(Entry(key, count)) }
  }
 
}

sealed trait StorageMessage
object Storage {
  case class Store(entry: Entry) extends StorageMessage
  case class Get(key: String, counterSeervice:ActorRef[CounterServiceMessage]) extends StorageMessage
  case class Entry(key: String, value: Long) extends CounterServiceMessage
  class StorageException(msg: String) extends RuntimeException(msg)
}
 
/**
 * Saves key/value pairs to persistent storage when receiving `Store` message.
 * Replies with current value when receiving `Get` message.
 * Will throw StorageException if the underlying data store is out of order.
 */
class Storage extends TypedActor[StorageMessage] {
  import Storage._
 
  val db = DummyDB
 
  def typedReceive = LoggingReceive {
    case Store(Entry(key, count)) => db.save(key, count)
    case Get(key, counterSeervice) => counterSeervice ! Entry(key, db.load(key).getOrElse(0L))
  }
}
 
object DummyDB {
  import Storage.StorageException
  private var db = Map[String, Long]()
 
  @throws(classOf[StorageException])
  def save(key: String, value: Long): Unit = synchronized {
    if (11 <= value && value <= 14) throw new StorageException("Simulated store failure " + value)
    db += (key -> value)
  }
 
  @throws(classOf[StorageException])
  def load(key: String): Option[Long] = synchronized {
    db.get(key)
  }
}