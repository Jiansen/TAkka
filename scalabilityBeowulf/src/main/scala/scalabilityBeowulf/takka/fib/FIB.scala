package scalabilityBeowulf.takka.fib

/*
Short description: Parallel execution

Arguments: N ( the number of processes )

Function: dom

Long description: Spawns N processes. Each process generates a list with 10000 
random integers between 1 and 200, sorts the list and then sends its first 
half to the parent process.
 */
import takka.actor.{TypedActor, ActorRef, ActorSystem, Props}
import akka.remote._

import util.BenchTimer
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

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

class FIBMaster extends TypedActor[FibMasterMsg] {
  val timer = new BenchTimer
  var n:Int = _
  def typedReceive = {
    case FibMasterStart(n) =>
      this.n = n
      val plist = (for (i<- 1 to n) yield {
        typedContext.actorOf(Props[GO, FIBWorker], ProcessNamePrefix+i)        
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
  private val nodes:Int = args(0).toInt
  private val processes:Int = 256

  private val system = ActorSystem("FIBSystem", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, processes, nodes))
  val testActor = system.actorOf(Props[FibMasterMsg, FIBMaster], ProcessPathPrefix)
  testActor ! FibMasterStart(processes)
}