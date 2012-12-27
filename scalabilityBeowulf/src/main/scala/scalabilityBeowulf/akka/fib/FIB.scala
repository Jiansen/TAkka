package scalabilityBeowulf.akka.fib

/*
Short description: Parallel execution

Arguments: N ( the number of processes )

Function: dom

Long description: Spawns N processes. Each process generates a list with 10000 
random integers between 1 and 200, sorts the list and then sends its first 
half to the parent process.
 */
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.remote._

import util.BenchTimer
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

sealed trait FibMasterMsg
case class FibMasterStart(n:Int) extends FibMasterMsg// number of processes
case class GO(master:ActorRef)
case class WorkerReply(worker:ActorRef, res:Int) extends FibMasterMsg

class FIBWorker extends Actor {
  def fib(n : Int):Int = n match {
    case 0 => 1
    case 1 => 1
    case x => fib(x-1) + fib(x-2)
  }
  
  def receive = {
    case GO(master) =>
      master ! WorkerReply(self, fib(40))
  }

}

class FIBMaster extends Actor {
  val timer = new BenchTimer
  var n:Int = _
  def receive = {
    case FibMasterStart(n) =>
      this.n = n
      val plist = (for (i<- 1 to n) yield {
        context.actorOf(Props[FIBWorker], ProcessNamePrefix+i)        
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
  private val nodes:Int = args(0).toInt
  private val processes:Int = 200

  private val system = ActorSystem("FIBSystem", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, processes, nodes))
  val testActor = system.actorOf(Props[FIBMaster], ProcessPathPrefix)
  testActor ! FibMasterStart(processes)
}