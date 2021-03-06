package scalabilityBeowulf.takka.bang

/*
 * A benchmark for many-to-one message passing that 
 * spawns one receiver and multiple senders that flood 
 * the receiver with messages. The benchmark is parameterized 
 * by the number of senders to spawn and the number of 
 * messages that each sender will send to the receiver.
 */

import takka.actor.{TypedActor, ActorRef, ActorSystem, Props}
import akka.remote._
import util.{BenchTimer, BenchCounter}
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._
import takka.chaos._
import scala.concurrent.duration._

import akka.actor.SupervisorStrategy._
import akka.actor.OneForOneStrategy

sealed trait BangMessage
case class BangBench(s:Int, m:Int) extends BangMessage
case object DummyMessage extends BangMessage
case object BangDone
case class Send(receiver:ActorRef[BangMessage], m:Int)

class Bang extends TypedActor[BangMessage]{  
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
      case e  =>
//        println("Error: "+e)
        Resume    
  }
  
  val timer = new BenchTimer
  val counter = new BenchCounter
  def typedReceive = {
    case BangBench(s, m) =>
      counter.set(s*m)
      val senders = (for (i<- 1 to s) yield {
        typedContext.actorOf(Props[Send, Sender], ProcessNamePrefix+i)
      }).toList
      
      if(util.Configuration.EnableChaos){
        import takka.chaos.ChaosMode._
        val chaos = ChaosMonkey(senders)
        chaos.setMode(Kill)
//        chaos.enableDebug
        chaos.start(1 second)
      }
      
      timer.start
      for (sender <- senders) {
        sender ! Send(typedSelf, m)
      }
    case DummyMessage => 
      counter.decrement
      if(util.Configuration.TraceProgress){
        println("Remaining messages"+counter.get)
      }
      if (counter.isZero){
        timer.finish
        timer.report
        sys.exit
      }
  }
}
  
class Sender extends TypedActor[Send]{
  // send m Done messages to receiver
//  print(self)
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
  private val processes:Int = 600
  private val messagess:Int = 2000

  private val system = ActorSystem("BangSystem", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, processes, nodes))
  val testActor = system.actorOf(Props[BangMessage, Bang], ProcessPathPrefix)
  testActor ! BangBench(processes,messagess)
}


