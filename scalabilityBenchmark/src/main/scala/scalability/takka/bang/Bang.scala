package scalability.takka.bang

/*
 * A benchmark for many-to-one message passing that 
 * spawns one receiver and multiple senders that flood 
 * the receiver with messages. The benchmark is parameterized 
 * by the number of senders to spawn and the number of 
 * messages that each sender will send to the receiver.
 */

import takka.actor.{TypedActor, ActorRef, ActorSystem, Props}
import util.{BenchTimer, BenchCounter}

sealed trait BangMessage
case class BangBench(s:Int, m:Int) extends BangMessage
case object DummyMessage extends BangMessage
case object BangDone
case class Send(receiver:ActorRef[BangMessage], m:Int)

class Bang extends TypedActor[BangMessage]{  
  val timer = new BenchTimer
  var counter = new BenchCounter
  def typedReceive = {
    case BangBench(s, m) =>
      counter.set(s*m)
      val senders = (for (i<- 1 to s) yield {
        typedContext.actorOf(Props[Send, Sender])
      }).toList
      timer.start
      for (sender <- senders) {
        sender ! Send(typedSelf, m)
      }
    case DummyMessage => 
      counter.decrement
        if (counter.isZero){
        timer.finish
        timer.report
        sys.exit
      }
  }
}
  
class Sender extends TypedActor[Send]{
  // send m Done messages to receiver
  def typedReceive = {
    case Send(receiver, m) => 
      var i:Int = 0;
      while(i<m){
        receiver ! DummyMessage
        i += 1
      }
    }
}
  
object BangBench extends App{
  private val processes:Int = 6000
  private val messagess:Int = 2000

  private val system = ActorSystem("BangSystem")
  val testActor = system.actorOf(Props[BangMessage, Bang], "BangBenchActor")
  testActor ! BangBench(processes,messagess)
}