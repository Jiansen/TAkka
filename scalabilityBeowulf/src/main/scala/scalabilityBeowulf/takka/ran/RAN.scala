package scalabilityBeowulf.takka.ran

/*
Short description: Parallel execution

Arguments: N ( the number of processes )

Function: dom

Long description: Spawns N processes. Each process generates a list with 100000 
random integers between 1 and 2000000, sorts the list and then sends its first 
half to the parent process.
 */
import takka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.remote._

import util.BenchTimer
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

sealed trait RANTestMessage
case class RANTestMsg(n:Int) extends RANTestMessage// number of processes
case class RANReply(process:ActorRef[RANProcessMessage], res:List[Int]) extends RANTestMessage
sealed trait RANProcessMessage
case class GO(testor:ActorRef[RANTestMessage]) extends RANProcessMessage


class RANProcess extends Actor[RANProcessMessage] {
  def n_rands(len : Int, max:Int):List[Int] = {
   var r = new scala.util.Random
   1 to len map { _ => r.nextInt(max) +1 } toList
  }
  
  def random(len:Int):List[Int] = {
    n_rands(len, 2000000).sortWith(_<_).take(len / 2)
  }
  
  def typedReceive = {
    case GO(testor) =>
      testor ! RANReply(typedSelf, random(100000))
  }

}

class RANTestActor extends Actor[RANTestMessage] {
  val timer = new BenchTimer
  var n:Int = _
  def typedReceive = {
    case RANTestMsg(n) =>
      this.n = n
      val plist = (for (i<- 1 to n) yield {
        typedContext.actorOf(Props[RANProcessMessage, RANProcess], ProcessNamePrefix+i)       
      }).toList
      timer.start
      
      for (p<-plist){        
        p ! GO(typedSelf)
      }
    case RANReply(_, _) =>
      this.n -= 1
      if (this.n == 0) {
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
  val testActor = system.actorOf(Props[RANTestMessage ,RANTestActor], ProcessPathPrefix)
  testActor ! RANTestMsg(processes)
}
