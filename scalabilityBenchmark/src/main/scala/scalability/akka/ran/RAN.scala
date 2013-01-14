package scalability.akka.ran

/*
Short description: Parallel execution

Arguments: N ( the number of processes )

Function: dom

Long description: Spawns N processes. Each process generates a list with 10000 
random integers between 1 and 200, sorts the list and then sends its first 
half to the parent process.
 */
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.BenchTimer
import language.postfixOps

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
      testor ! RANReply(self, random(1000000))
  }

}

class RANTestActor extends Actor {
  val timer = new BenchTimer
  var n:Int = _
  def receive = {
    case RANTestMsg(n) =>
      this.n = n
      val plist = (for (i<- 1 to n) yield {
        context.actorOf(Props[RANProcess])        
      }).toList
      timer.start
      
      for (p<-plist){
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
  private val processes:Int = 6000
 
  private val system = ActorSystem("RANSystem")
  val testActor = system.actorOf(Props[RANTestActor], "RANTestActor")
  testActor ! RANTestMsg(processes)
}