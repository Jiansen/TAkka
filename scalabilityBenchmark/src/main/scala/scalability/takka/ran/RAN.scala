package scalability.takka.ran

/*
Short description: Parallel execution

Arguments: N ( the number of processes )

Function: dom

Long description: Spawns N processes. Each process generates a list with 10000 
random integers between 1 and 200, sorts the list and then sends its first 
half to the parent process.
 */
import takka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.BenchTimer

sealed trait TestorMsg

case class RANTestMsg(n:Int) extends TestorMsg// number of processes
case class GO(testor:ActorRef[TestorMsg])
case class RANReply(process:ActorRef[GO], res:List[Int]) extends TestorMsg

class RANProcess extends Actor[GO] {
  def n_rands(len : Int, max:Int):List[Int] = {
   var r = new scala.util.Random
   1 to len map { _ => r.nextInt(max) +1 } toList
  }
  
  def random(n:Int):List[Int] = {
    val len = 100000
    n_rands(len, 200).sortWith(_<_).take(len / 2)
  }
  
  def typedReceive = {
    case GO(testor) =>
      testor ! RANReply(typedSelf, random(100))    
  }

}

class RANTestActor extends Actor[TestorMsg] {
  val timer = new BenchTimer
  var n:Int = _
  def typedReceive = {
    case RANTestMsg(n) =>
      this.n = n
      val plist = (for (i<- 1 to n) yield {
        typedContext.actorOf(Props[GO, RANProcess])        
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
      /*
    Start = now(),
    lists:foreach(fun (P) -> P ! {Parent, go} end, PList),
    lists:foreach(fun (P) -> receive {P, _RN} -> ok end end, PList),
    Stop = now(),
    timer:now_diff(Stop, Start)/1000.
       */
      
  }
}

object RAN extends App {
  private val system = ActorSystem("RANSystem")

  val testActor = system.actorOf(Props[TestorMsg, RANTestActor], "RANTestActor")
  testActor ! RANTestMsg(200) // 200 processes
}