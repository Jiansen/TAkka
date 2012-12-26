package scalabilityBeowulf.takka.bang

/*
 * A benchmark for many-to-one message passing that 
 * spawns one receiver and multiple senders that flood 
 * the receiver with messages. The benchmark is parameterized 
 * by the number of senders to spawn and the number of 
 * messages that each sender will send to the receiver.
 */

import takka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.remote._
import scala.concurrent.ops.spawn
import util.{BenchTimer, BenchCounter}
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

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
  private val nodes:Int = args(0).toInt
  private val processes:Int = 6000

  private val system = ActorSystem("BangSystem", masterNodeConfig(BangNodeConfig.WorkerNodePrefix, BangNodeConfig.ProcessPathPrefix, BangNodeConfig.ProcessNamePrefix, processes, nodes))
  val testActor = system.actorOf(Props[BangMessage, Bang], "BangBenchActor")
  testActor ! BangBench(6000,2000)
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