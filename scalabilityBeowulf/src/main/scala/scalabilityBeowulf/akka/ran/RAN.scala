package scalabilityBeowulf.akka.ran

/*
Short description: Parallel execution

Arguments: N ( the number of processes )

Function: dom

Long description: Spawns N processes. Each process generates a list with 100000 
random integers between 1 and 2000000, sorts the list and then sends its first 
half to the parent process.
 */
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.remote._

import util.{BenchTimer, BenchCounter}
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._
import language.postfixOps

import akka.chaos._
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy._
import akka.actor.OneForOneStrategy

case class RANTestMsg(n:Int)// number of processes
case class GO(testor:ActorRef)
case class RANReply(process:ActorRef, res:List[Int])

class RANProcess extends Actor {
  def n_rands(len : Int, max:Int):List[Int] = {
   var r = new scala.util.Random
   1 to len map { _ => r.nextInt(max) +1 } toList
  }
  
  def random(len:Int):List[Int] = {
    n_rands(len, 2000000).sortWith(_<_).take(len / 2)
  }
  
  def receive = {
    case GO(testor) =>
      testor ! RANReply(self, random(100000))
  }

}

class RANTestActor extends Actor {
  val timer = new BenchTimer
  val counter = new BenchCounter
  
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
      case e  =>
        Resume    
  }
  
  def receive = {
    case RANTestMsg(n) =>
      counter.set(n)
      val plist = (for (i<- 1 to n) yield {
        context.actorOf(Props[RANProcess], ProcessNamePrefix+i)
      }).toList
      
      if(util.Configuration.EnableChaos){
        import akka.chaos.ChaosMode._
        val chaos = ChaosMonkey(plist)
        chaos.setMode(Kill)
//        chaos.enableDebug
        chaos.start(1 second)
      }
      
      timer.start
      
      for (p<-plist){
        p ! GO(self)
      }
    case RANReply(_, _) =>
      counter.decrement
      if(util.Configuration.TraceProgress){
          println("Remaining processes: "+counter.get)
        }
      if (counter.isZero) {
        timer.finish
        timer.report
        sys.exit
      }
  }
}

object RAN extends App {
  private val nodes:Int = args(0).toInt
  private val processes:Int = 6000

  private val system = ActorSystem("RANSystem", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, processes, nodes))
  val testActor = system.actorOf(Props[RANTestActor], ProcessPathPrefix)
  testActor ! RANTestMsg(processes)
}
