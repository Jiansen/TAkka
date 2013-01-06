package scalability.akka.timerwheel
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
import util.BenchTimer
import akka.actor.ReceiveTimeout
import scala.concurrent.duration._
//import scala.concurrent.duration._

case class Wheel(n:Int)
case class NoWheel(n:Int)

case class Ping(pid:ActorRef)
case class Pong(pid:ActorRef)

case class Init(pids:List[ActorRef])
case object Start
case class Done(pid:ActorRef)

class WheelHandler(ping_left:Int, master:ActorRef) extends Actor {//with timeout
  context.setReceiveTimeout(30 milliseconds)
  
  var others:List[ActorRef] = _
  var pingLeft = ping_left 
  def receive = {
    case Init(others) =>
      this.others = others
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

class NoWheelHandler(ping_left:Int, master:ActorRef) extends Actor {//without timeout
  var others:List[ActorRef] = _
  var pingLeft = ping_left 
  def receive = {
    case Init(others) =>
      this.others = others
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
          context.actorOf(Props[WheelHandler].withCreator(new WheelHandler(n-1, self)))) toList
          
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
          context.actorOf(Props[NoWheelHandler].withCreator(new NoWheelHandler(n-1, self)))) toList
          
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

  val testActor = system.actorOf(Props[TimerWheelActor], "TimerWheelTestActor")
  //testActor ! Wheel(8000)
  testActor ! NoWheel(800)
}