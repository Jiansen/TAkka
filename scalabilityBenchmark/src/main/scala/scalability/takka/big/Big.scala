package scalability.takka.big

/*
A benchmark that implements a many-to-many message passing scenario. 
Several processes are spawned, each of which sends a ping message to 
the others, and responds with a pong message to any ping message it 
receives. The benchmark is parameterized by the number of processes.
 */
import takka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.BenchTimer

sealed trait PingerMsg
sealed trait ReporterMsg
case class BigDone(p:ActorRef[PingerMsg]) extends ReporterMsg
case class BigProcs(procs:List[ActorRef[PingerMsg]]) extends PingerMsg
case class BigPing(from:ActorRef[PingerMsg]) extends PingerMsg
case object BigPong extends PingerMsg

class Pinger(val reporter:ActorRef[ReporterMsg]) extends Actor[PingerMsg]{
  private var n:Int = _
  def typedReceive = {
    case BigProcs(procs) =>
      this.n = procs.length
      for (p <- procs) {
        p ! BigPing(typedSelf)
      }
    case BigPong =>
      n = n-1
      if (n == 0) {
        reporter ! BigDone(typedSelf)
      }
    case BigPing(from) => 
      from ! BigPong
  }
}

class Reporter(var n:Int, val timer:BenchTimer) extends Actor[ReporterMsg]{
  def typedReceive = {
    case BigDone(_) =>
      n = n-1
      if (n == 0) {
        timer.finish
        timer.report
        sys.exit
      }
  }
}

object Big extends App{
  private val timer = new BenchTimer
  private val system = ActorSystem("BigSystem")
  
  def spawn_procs(n:Int, reporter:ActorRef[ReporterMsg]):List[ActorRef[PingerMsg]] = {
    val procs = for (i<-1 to n) yield {
      system.actorOf(Props().withCreator(new Pinger(reporter)))
    }
    procs.toList
  }
  
  def send_procs(procs:List[ActorRef[PingerMsg]], msg:BigProcs) = {
    for (p<-procs){  p ! msg  }
  }
  
  def big(n:Int){
    val reporter = system.actorOf(Props[Reporter].withCreator(new Reporter(n, timer)))    
    val procs = spawn_procs(n, reporter)
    timer.start
    send_procs(procs, BigProcs(procs))
  }
  
  big(1500) // 2325
}