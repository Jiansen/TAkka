package scalabilityBeowulf.akka.ran

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


case class RANTestMsg(n:Int)// number of processes
case class GO(testor:ActorRef)
case class RANReply(process:ActorRef, res:List[Int])

class RANProcess extends Actor {
  def n_rands(len : Int, max:Int):List[Int] = {
   var r = new scala.util.Random
   1 to len map { _ => r.nextInt(max) +1 } toList
  }
  
  def random(n:Int):List[Int] = {
    val len = 100000
    n_rands(len, 200).sortWith(_<_).take(len / 2)
  }
  
  def receive = {
    case GO(testor) =>
      testor ! RANReply(self, random(100))    
      println(self.path)
  }

}

class RANTestActor extends Actor {
  val timer = new BenchTimer
  var n:Int = _
  def receive = {
    case RANTestMsg(n) =>
      this.n = n
      val plist = (for (i<- 1 to n) yield {
        context.actorOf(Props[RANProcess], RANNodeConfig.ProcessNamePrefix+i)       
      }).toList
      timer.start
      
      for (p<-plist){
//        println(p.path)
        p ! GO(self)
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
  private val processes:Int = 6

  private val system = ActorSystem("RANSystem", masterNodeConfig(RANNodeConfig.WorkerNodePrefix, RANNodeConfig.ProcessPathPrefix, RANNodeConfig.ProcessNamePrefix, processes, nodes))
  val testActor = system.actorOf(Props[RANTestActor], "RANTestActor")
  testActor ! RANTestMsg(processes)
}

object RANNode extends App {
  private val nodeID:Int = args(0).toInt

  private val system = ActorSystem(RANNodeConfig.WorkerNodePrefix+nodeID, WorkerNodeConfig(nodeID))
}

object RANNodeConfig {
  val WorkerNodePrefix = "RANNodeSystem"
  val ProcessPathPrefix = "RANTestActor"
  val ProcessNamePrefix = "RANProcess"
}