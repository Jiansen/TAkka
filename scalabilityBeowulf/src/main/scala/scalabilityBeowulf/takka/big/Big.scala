package scalabilityBeowulf.takka.big

/*
A benchmark that implements a many-to-many message passing scenario. 
Several processes are spawned, each of which sends a ping message to 
the others, and responds with a pong message to any ping message it 
receives. The benchmark is parameterized by the number of processes.
 */
import takka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.{BenchTimer, BenchCounter}
import akka.remote._
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

sealed trait PingerMsg
sealed trait ReporterMsg
case class BigBench(processes:Int) extends ReporterMsg
case class BigDone(p:ActorRef[PingerMsg]) extends ReporterMsg
case class BigProcs(procs:List[ActorRef[PingerMsg]], reporter:ActorRef[ReporterMsg]) extends PingerMsg
case class BigPing(from:ActorRef[PingerMsg]) extends PingerMsg
case object BigPong extends PingerMsg

class Pinger extends Actor[PingerMsg]{
  private var n = new BenchCounter
  private var reporter:ActorRef[ReporterMsg] = _
  def typedReceive = {
    case BigProcs(procs, reporter) =>
      this.reporter = reporter
      n.set(procs.length)
      for (p <- procs) {
        p ! BigPing(typedSelf)
      }
    case BigPong =>
      n.decrement
      if (n.isZero) {
        reporter ! BigDone(typedSelf)
      }
    case BigPing(from) => 
      from ! BigPong
  }
}

class Reporter extends Actor[ReporterMsg]{
  private val timer = new BenchTimer
  val counter = new BenchCounter
  def typedReceive = {
    case BigBench(n) => {
      counter.set(n)
      val procs = (for (i<-1 to n) yield {
        typedContext.actorOf(Props[PingerMsg, Pinger], ProcessNamePrefix+i)
      }).toList
      timer.start
      for (p<-procs){  p ! BigProcs(procs, typedSelf)  }
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
  private val processes = 1024
  
  
  private val system = ActorSystem("BigSystem", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, processes, nodes))  
  val reporter = system.actorOf(Props[ReporterMsg, Reporter], ProcessPathPrefix)
  reporter ! BigBench(processes)
}
