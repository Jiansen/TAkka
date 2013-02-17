package scalability.takka.fib

/*
Short description: Parallel execution

Arguments: N ( the number of processes )

Function:

Long description: Spawns N processes. Each process calculate fib(x) and
 send the result to the parent process.
 */
import takka.actor.{TypedActor, ActorRef, ActorSystem, Props}
import util.BenchTimer

sealed trait FibMasterMsg
case class FibMasterStart(n:Int) extends FibMasterMsg// number of processes
case class GO(master:ActorRef[WorkerReply])
case class WorkerReply(worker:ActorRef[GO], res:Int) extends FibMasterMsg

class FIBWorker extends TypedActor[GO] {
  def fib(n : Int):Int = n match {
    case 0 => 1
    case 1 => 1
    case x => fib(x-1) + fib(x-2)
  }
  
  def typedReceive = {
    case GO(master) =>
      master ! WorkerReply(typedSelf, fib(40))
  }

}

class FIBMaster extends TypedActor [FibMasterMsg] {
  val timer = new BenchTimer
  var n:Int = _
  def typedReceive = {
    case FibMasterStart(n) =>
      this.n = n
      val plist = (for (i<- 1 to n) yield {
        typedContext.actorOf(Props[GO, FIBWorker])        
      }).toList
      timer.start
      
      for (p<-plist){
        p ! GO(typedSelf)
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
  private val processes:Int = 100
 
  private val system = ActorSystem("FIBSystem")
  val testActor = system.actorOf(Props[FibMasterMsg, FIBMaster], "FIBMaster")
  testActor ! FibMasterStart(processes)
}