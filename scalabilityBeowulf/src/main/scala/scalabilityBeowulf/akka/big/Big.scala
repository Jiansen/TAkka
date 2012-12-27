package scalabilityBeowulf.akka.big

/*
A benchmark that implements a many-to-many message passing scenario. 
Several processes are spawned, each of which sends a ping message to 
the others, and responds with a pong message to any ping message it 
receives. The benchmark is parameterized by the number of processes.
 */
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.{BenchTimer, BenchCounter}
import akka.remote._
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

sealed trait PingerMsg
sealed trait ReporterMsg
case class BigBench(processes:Int) extends ReporterMsg
case class BigDone(p:ActorRef) extends ReporterMsg
case class BigProcs(procs:List[ActorRef], reporter:ActorRef) extends PingerMsg
case class BigPing(from:ActorRef) extends PingerMsg
case object BigPong extends PingerMsg

class Pinger extends Actor{
  private var n = new BenchCounter
  private var reporter:ActorRef = _
  def receive = {
    case BigProcs(procs, reporter) =>
      this.reporter = reporter
      n.set(procs.length)
      for (p <- procs) {
        p ! BigPing(self)
      }
    case BigPong =>
      n.decrement
      if (n.isZero) {
        reporter ! BigDone(self)
      }
    case BigPing(from) => 
      from ! BigPong
  }
}

class Reporter extends Actor{
  private val timer = new BenchTimer
  val counter = new BenchCounter
  def receive = {
    case BigBench(n) => {
      counter.set(n)
      val procs = (for (i<-1 to n) yield {
        context.actorOf(Props[Pinger], BigNodeConfig.ProcessNamePrefix+i)
      }).toList
      timer.start
      for (p<-procs){  p ! BigProcs(procs, self)  }
    }
    case BigDone(_) =>
      counter.decrement
      if (counter.isZero) {
        timer.finish
        timer.report
        sys.exit
      }
  }
}

object BigBench extends App{
  private val nodes:Int = args(0).toInt
  private val processes = 15
  
  
  private val system = ActorSystem("BigSystem", masterNodeConfig(WorkerNodePrefix, BigNodeConfig.ProcessPathPrefix, BigNodeConfig.ProcessNamePrefix, processes, nodes))  
  val reporter = system.actorOf(Props[Reporter], "BigBenchActor")
  reporter ! BigBench(processes)
}

object BigNodeConfig {
  val ProcessPathPrefix = "BigBenchActor"
  val ProcessNamePrefix = "BigProcess"
}