package scalabilityBeowulf.takka.parallel

/*
A benchmark for parallel execution that spawns a number of processes, 
each of which creates a list of N timestamps and, after it checks that 
each element of the list is strictly greater than its previous one (as 
promised by the implementation of erlang:now/0), it sends the result to 
its parent. The benchmark is parameterized by the number of processes 
and the number of timestamps.
 */
import takka.actor.{TypedActor, ActorRef, ActorSystem, Props}
import util.{BenchTimer, BenchCounter}
import akka.remote._
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

import takka.chaos._
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy._
import akka.actor.OneForOneStrategy

case object OK
sealed trait MasterMsg
case class Start(n:Int, m:Int) extends MasterMsg
case class Result(pid:ActorRef[Loop], r:Boolean) extends MasterMsg
case class Loop(master:ActorRef[MasterMsg],n:Int)

class NowTime extends TypedActor[MasterMsg] {  
  val counter = new BenchCounter
  
  val timer = new BenchTimer  
  
    override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
      case e  =>
        Resume    
  }
  
  def typedReceive = {
    case Start(n:Int, m:Int) =>
      counter.set(m)
      val me = self
      val base = for (_ <- 1 to m) yield OK      
      timer.start
      val pids = (for (i <- 1 to m) yield {
        typedContext.actorOf(Props[Loop, LoopActor], ProcessNamePrefix+i)
      }).toList
      
      if(util.Configuration.EnableChaos){
          import takka.chaos.ChaosMode._
          val chaos = ChaosMonkey(pids)
          chaos.setMode(Kill)
//          chaos.enableDebug
          chaos.start(1 second)
        }
        
      
      for (pid <- pids) {
        pid ! Loop(typedSelf, n)
      }
    case Result(_, _) =>
      counter.decrement
      if(util.Configuration.TraceProgress){
          println("Remaining processes: "+counter.get)
        }
      if (counter.isZero){
        timer.finish
        timer.report
        sys.exit()
      }
  }
}

class LoopActor extends TypedActor[Loop] {
  def check_now(ts:List[Long]):Boolean = {
    true
  }
  def loop(n:Int, out:List[Long]):Boolean = {
    if (n == 0){
      check_now(out)
    }else{
      loop(n-1, System.currentTimeMillis()::out) 
    }
  }
  
  def typedReceive = {
    case Loop(master,n:Int) =>
      master ! Result(typedSelf, loop(n, Nil))  
    case _ => 
  }
}


object ParallelBench extends App{
  private val nodes:Int = args(0).toInt
  private val processes = 256
  
  private val system = ActorSystem("ParallelSystem", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, processes, nodes))  
  val master = system.actorOf(Props[MasterMsg, NowTime], ProcessPathPrefix)
  master ! Start(6000, processes)
}
