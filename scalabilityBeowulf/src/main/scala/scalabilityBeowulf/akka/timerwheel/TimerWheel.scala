package scalabilityBeowulf.akka.timerwheel
/*
Short description: Timer management

Arguments: N ( the number of processes )

Function: wheel, no_wheel

Long description: Spawns N processes. Each process sends a "ping" message to 
all other processes and then waits to receive a "pong" message from it (with 
or without a timeout), before sending out any more "ping" messages. While 
waiting for a "pong" message, the process can respond with a "pong" message to 
any "ping" messages it receives. In case of a timeout, the process exits.
 */
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.actor.ReceiveTimeout
import akka.remote._
import akka.util.duration._
import util.BenchTimer
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

sealed trait MasterMsg
sealed trait WheelMsg


case class Wheel(n:Int) extends MasterMsg
case class NoWheel(n:Int) extends MasterMsg

case class Ping(pid:ActorRef) extends WheelMsg
case class Pong(pid:ActorRef) extends WheelMsg

case class Init(pids:List[ActorRef], ping_left:Int, master:ActorRef) extends WheelMsg
case object Start extends WheelMsg
case class Done(pid:ActorRef) extends MasterMsg

class WheelHandler extends Actor {//with timeout
  context.setReceiveTimeout(30 milliseconds)
  
  var others:List[ActorRef] = _
  var pingLeft:Int = _
  var master:ActorRef = _
  def receive = {
    case Init(others, ping_left, master) =>
      this.others = others
      this.pingLeft = ping_left
      this.master = master
    case Start =>
      for (p<-others) {p ! Ping(self)}
    case Pong(_) => //ignore
    case Ping(other) =>
      other ! Pong(self)
      pingLeft -= 1
      if(pingLeft == 0) {
        master ! Done(self)
      }
    case ReceiveTimeout =>
      master ! Done(self)  
  }
}

class NoWheelHandler extends Actor {//without timeout
  var others:List[ActorRef] = _
  var pingLeft:Int = _
  var master:ActorRef = _
  
  def receive = {
    case Init(others, ping_left, master) =>
      this.others = others
      this.pingLeft = ping_left
      this.master = master
    case Start =>
      for (p<-others) {p ! Ping(self)}
    case Pong(_) => //ignore
    case Ping(other) =>
      other ! Pong(self)
      pingLeft -= 1
      if(pingLeft == 0) {
        master ! Done(self)
      }
  }
}

class TimerWheelActor extends Actor {
  val timer = new BenchTimer
  var n:Int = _
  
  def receive = {
    case Wheel(n) =>
      this.n = n
      // val me = self
      val pids = (for (i <- 1 to n) yield 
          context.actorOf(Props[WheelHandler], TimerWheelNodeConfig.ProcessNamePrefix+i)) toList
          
      for (pid <- pids) {
        pid ! Init(pids filterNot (_ == pid), n-1, self)
      } 
      timer.start
      for (pid <- pids) {
        pid ! Start
      } 
      
    case NoWheel(n) =>      
      this.n = n
      // val me = self
      val pids = (for (i <- 1 to n) yield 
          context.actorOf(Props[NoWheelHandler], TimerWheelNodeConfig.ProcessNamePrefix+i)) toList
          
      for (pid <- pids) {
        pid ! Init(pids filterNot (_ == pid), n-1, self)
      } 
      timer.start
      for (pid <- pids) {
        pid ! Start
      }       
      
    case Done(pid) =>
      this.n -= 1
      if (this.n == 0) {
        this.timer.finish
        this.timer.report
        sys.exit()
      }
  }
}

object TimerWheelBench extends App {
  private val nodes:Int = args(0).toInt
  private val processes:Int = 800

  private val system = ActorSystem("TimerWheelSystem", masterNodeConfig(WorkerNodePrefix, TimerWheelNodeConfig.ProcessPathPrefix, TimerWheelNodeConfig.ProcessNamePrefix, processes, nodes))
  val testActor = system.actorOf(Props[TimerWheelActor], "TimerWheelActor")
  //testActor ! Wheel(processes)
  testActor ! NoWheel(processes)
}

object TimerWheelNodeConfig {
  val ProcessPathPrefix = "TimerWheelActor"
  val ProcessNamePrefix = "TimerWheelProcess"
}
