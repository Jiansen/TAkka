package scalability.takka.parallel

/*
A benchmark for parallel execution that spawns a number of processes, 
each of which creates a list of N timestamps and, after it checks that 
each element of the list is strictly greater than its previous one (as 
promised by the implementation of erlang:now/0), it sends the result to 
its parent. The benchmark is parameterized by the number of processes 
and the number of timestamps.
 */
import takka.actor.{TypedActor, ActorRef, ActorSystem, Props}
import util.BenchTimer
import util.BenchCounter

object OK
object Start
case class Result(pid:ActorRef[LoopMsg], r:Boolean)
sealed trait LoopMsg

class NowTime(n:Int, m:Int) extends TypedActor[Result] {  
  val counter = new BenchCounter
  counter.set(m)
  val timer = new BenchTimer  
  val me = self
  val base = for (_ <- 1 to m) yield OK      
  timer.start
  val pids = for (_ <- 1 to m) yield {
    typedContext.actorOf(Props[LoopMsg].withCreator(new LoopActor(typedSelf, n)))
  }  
  def typedReceive = {
    case Result(_, _) =>
      counter.decrement
      if (counter.isZero){
        timer.finish
        timer.report
        sys.exit()
      }
  }
}

class LoopActor(master:ActorRef[Result],n:Int) extends TypedActor[LoopMsg] {
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
  master ! Result(typedSelf, loop(n, Nil))  
  def typedReceive = {
    case _ => 
  }
}

object Parallel extends App{
  val system = ActorSystem("parallel")
  system.actorOf(Props().withCreator(new NowTime(5000, 128)));
}


/*
loop(Pid, 0, Out) -> Pid ! {self(), check_now(Out)};
loop(Pid, N, Out) -> loop(Pid, N - 1, [now()|Out]).

check_now([_,_]) -> ok;
check_now([_|Ts]) -> check_now(Ts).
*/