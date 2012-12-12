package scalability.takka.bang

/*
 * A benchmark for many-to-one message passing that 
 * spawns one receiver and multiple senders that flood 
 * the receiver with messages. The benchmark is parameterized 
 * by the number of senders to spawn and the number of 
 * messages that each sender will send to the receiver.
 */

import takka.actor.{Actor, ActorRef, ActorSystem, Props}
import scala.concurrent.ops.spawn
import util.{BenchTimer, BenchCounter}

sealed trait BangMessage
case class BangBench(s:Int, m:Int) extends BangMessage
object DummyMessage extends BangMessage
object BangDone

class Bang extends Actor[BangMessage]{  
  val timer = new BenchTimer
  var counter = new BenchCounter
  def typedReceive = {
    case BangBench(s, m) =>
      counter.set(s*m)
      val senders = for (i<- 1 to s) yield {
        new Sender
      }
      timer.start
      for (sender <- senders) spawn {
        sender.send(typedSelf, m)
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
  
class Sender {
  // send m Done messages to receiver
  def send(receiver:ActorRef[DummyMessage.type], m:Int) = {
    var i:Int = 0;
    while(i<m){
      receiver ! DummyMessage
      i += 1
    }
  }
}
  
object BangBench extends App{  
  def bang(s:Int, m:Int){
    val system = ActorSystem("BangSystem")
    val bang = system.actorOf(Props[BangMessage, Bang], "receiver")
    bang ! BangBench(s, m)
  }
  
  bang(6000,2000)
}