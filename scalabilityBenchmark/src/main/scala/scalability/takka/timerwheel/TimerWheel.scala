package scalability.takka.timerwheel
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
import takka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.BenchTimer
import takka.actor.ReceiveTimeout
import akka.util.duration._
//import scala.concurrent.duration._

sealed trait MasterMsg
sealed trait WheelMsg


case class Wheel(n:Int) extends MasterMsg
case class NoWheel(n:Int) extends MasterMsg

case class Ping(pid:ActorRef[WheelMsg]) extends WheelMsg
case class Pong(pid:ActorRef[WheelMsg]) extends WheelMsg

case class Init(pids:List[ActorRef[WheelMsg]]) extends WheelMsg
case object Start extends WheelMsg
case class Done(pid:ActorRef[WheelMsg]) extends MasterMsg

class WheelHandler(ping_left:Int, master:ActorRef[MasterMsg]) extends Actor[WheelMsg] {//with timeout
  context.setReceiveTimeout(30 milliseconds)
  
  var others:List[ActorRef[WheelMsg]] = _
  var pingLeft = ping_left 
  def typedReceive = {
    case Init(others) =>
      this.others = others
    case Start =>
      for (p<-others) {p ! Ping(typedSelf)}
    case Pong(_) => //ignore
    case Ping(other) =>
      other ! Pong(typedSelf)
      pingLeft -= 1
      if(pingLeft == 0) {
        master ! Done(typedSelf)
      }
  }
  
  override def systemMessageHandler = {
    case ReceiveTimeout =>
      master ! Done(typedSelf)
  }
}

class NoWheelHandler(ping_left:Int, master:ActorRef[MasterMsg]) extends Actor[WheelMsg] {//without timeout
  var others:List[ActorRef[WheelMsg]] = _
  var pingLeft = ping_left 
  def typedReceive = {
    case Init(others) =>
      this.others = others
    case Start =>
      for (p<-others) {p ! Ping(typedSelf)}
    case Pong(_) => //ignore
    case Ping(other) =>
      other ! Pong(typedSelf)
      pingLeft -= 1
      if(pingLeft == 0) {
        master ! Done(typedSelf)
      }
  }
}

class TimerWheelActor extends Actor[MasterMsg] {
  val timer = new BenchTimer
  var n:Int = _
  
  def typedReceive = {
    case Wheel(n) =>
      this.n = n
      // val me = self
      val pids = (for (i <- 1 to n) yield 
          typedContext.actorOf(Props[WheelMsg, WheelHandler].withCreator(new WheelHandler(n-1, typedSelf)))) toList
          
      for (pid <- pids) {
        pid ! Init(pids filterNot (_ == pid))
      } 
      timer.start
      for (pid <- pids) {
        pid ! Start
      } 
      
    case NoWheel(n) =>      
      this.n = n
      // val me = self
      val pids = (for (i <- 1 to n) yield 
          typedContext.actorOf(Props[NoWheelHandler].withCreator(new NoWheelHandler(n-1, typedSelf)))) toList
          
      for (pid <- pids) {
        pid ! Init(pids filterNot (_ == pid))
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

object TimerWheel extends App{
  private val system = ActorSystem("TimerWheelSystem")

  val testActor = system.actorOf(Props[MasterMsg, TimerWheelActor], "TimerWheelTestActor")
  //testActor ! Wheel(8000)
  testActor ! NoWheel(800)
}