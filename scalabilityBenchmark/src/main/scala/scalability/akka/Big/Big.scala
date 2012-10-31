package scalability.akka.big

/*
A benchmark that implements a many-to-many message passing scenario. 
Several processes are spawned, each of which sends a ping message to 
the others, and responds with a pong message to any ping message it 
receives. The benchmark is parameterized by the number of processes.
 */
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.BenchTimer

case class BigDone(p:ActorRef)
case class BigProcs(procs:List[ActorRef])
case class BigPing(from:ActorRef)
case object BigPong

class Pinger(val reporter:ActorRef) extends Actor{
  private var n:Int = _
  def receive = {
    case BigProcs(procs) =>
      this.n = procs.length
      for (p <- procs) {
        p ! BigPing(self)
      }
    case BigPong =>
      n = n-1
      if (n == 0) {
        reporter ! BigDone(self)
      }
    case BigPing(from) => 
      from ! BigPong
  }
}

class Reporter(var n:Int, val timer:BenchTimer) extends Actor{
  def receive = {
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
  
  def spawn_procs(n:Int, reporter:ActorRef):List[ActorRef] = {
    val procs = for (i<-1 to n) yield {
      system.actorOf(Props().withCreator(new Pinger(reporter)))
    }
    procs.toList
  }
  
  def send_procs(procs:List[ActorRef], msg:Any) = {
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