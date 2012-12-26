package scalabilityBeowulf.akka.bang

/*
 * A benchmark for many-to-one message passing that 
 * spawns one receiver and multiple senders that flood 
 * the receiver with messages. The benchmark is parameterized 
 * by the number of senders to spawn and the number of 
 * messages that each sender will send to the receiver.
 */

import takka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.remote._
import util.{BenchTimer, BenchCounter}
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

sealed trait BangMessage
case class BangBench(s:Int, m:Int) extends BangMessage
case object DummyMessage extends BangMessage
case object BangDone
case class Send(receiver:ActorRef[BangMessage], m:Int)

class Bang extends Actor[BangMessage]{  
  val timer = new BenchTimer
  val counter = new BenchCounter
  def typedReceive = {
    case BangBench(s, m) =>
      counter.set(s*m)
      val senders = (for (i<- 1 to s) yield {
        typedContext.actorOf(Props[Send, Sender], BangNodeConfig.ProcessNamePrefix+i)
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
  
class Sender extends Actor[Send]{
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
  private val nodes:Int = args(0).toInt
  private val processes:Int = 6000
  private val messagess:Int = 2000

  private val system = ActorSystem("BangSystem", masterNodeConfig(BangNodeConfig.WorkerNodePrefix, BangNodeConfig.ProcessPathPrefix, BangNodeConfig.ProcessNamePrefix, processes, nodes))
  val testActor = system.actorOf(Props[BangMessage, Bang], "BangBenchActor")
  testActor ! BangBench(processes,messagess)
}

object BangNode extends App {
  private val nodeID:Int = args(0).toInt

  private val system = ActorSystem(BangNodeConfig.WorkerNodePrefix+nodeID, WorkerNodeConfig(nodeID))
}

object BangNodeConfig {
  val WorkerNodePrefix = "BangNode"
  val ProcessPathPrefix = "BangBenchActor"
  val ProcessNamePrefix = "BangProcess"
}