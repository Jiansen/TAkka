package scalability.akka.fib

/*
Short description: Parallel execution

Arguments: N ( the number of processes )

Function:

Long description: Spawns N processes. Each process calculate fib(x) and
 send the result to the parent process.
 */
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.BenchTimer

case class FibMasterStart(n:Int)// number of processes
case class GO(master:ActorRef)
case class WorkerReply(worker:ActorRef, res:Int)

class FIBWorker extends Actor {
  def fib(n : Int):Int = n match {
    case 0 => 1
    case 1 => 1
    case x => fib(x-1) + fib(x-2)
  }
  
  def receive = {
    case GO(master) =>
      master ! WorkerReply(self, fib(35))
  }

}

class FIBMaster extends Actor {
  val timer = new BenchTimer
  var n:Int = _
  def receive = {
    case FibMasterStart(n) =>
      this.n = n
      val plist = (for (i<- 1 to n) yield {
        context.actorOf(Props[FIBWorker])        
      }).toList
      timer.start
      
      for (p<-plist){
        p ! GO(self)
      }
    case WorkerReply(_, _) =>
      this.n -= 1
      if (this.n == 0) {
        timer.finish
        timer.report
        sys.exit
      }
  }
}

object FIB extends App {
  private val processes:Int = 200
 
  private val system = ActorSystem("RANSystem")
  val testActor = system.actorOf(Props[FIBMaster], "FibMaster")
  testActor ! FibMasterStart(processes)
}