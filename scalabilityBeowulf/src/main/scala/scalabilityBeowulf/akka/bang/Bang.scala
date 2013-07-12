package scalabilityBeowulf.akka.bang

/*
 * A benchmark for many-to-one message passing that 
 * spawns one receiver and multiple senders that flood 
 * the receiver with messages. The benchmark is parameterized 
 * by the number of senders to spawn and the number of 
 * messages that each sender will send to the receiver.
 */

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.remote._
import util.{BenchTimer, BenchCounter}
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

sealed trait BangMessage
case class BangBench(s:Int, m:Int) extends BangMessage
case object DummyMessage extends BangMessage
case object BangDone
case class Send(receiver:ActorRef, m:Int)

import scala.concurrent.duration._
import akka.actor.SupervisorStrategy._
import akka.actor.OneForOneStrategy
import akka.chaos._

class Bang extends Actor{  
   override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
      case e  =>
        Resume    
  }
    
  val timer = new BenchTimer
  val counter = new BenchCounter
  def receive = {
    case BangBench(s, m) =>
      counter.set(s*m)
      val senders = (for (i<- 1 to s) yield {
        context.actorOf(Props[Sender], ProcessNamePrefix+i)
      }).toList
      
      if(util.Configuration.EnableChaos){
        import akka.chaos.ChaosMode._
        val chaos = ChaosMonkey(senders)
        chaos.setMode(Kill)
//        chaos.enableDebug
        chaos.start(1 second)
      }
      
      timer.start
      for (sender <- senders) {
//println("sending message form "+self+" to "+sender)        
        sender ! Send(self, m)
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
  
class Sender extends Actor{
  // send m Done messages to receiver
  def receive = {
    case Send(receiver, m) => 
      var i:Int = 0;
      while(i<m){
//println(self+" reply message to "+receiver)        
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
// println("OK?"+nodes)

  val testActor = system.actorOf(Props[Bang], ProcessPathPrefix)
  testActor ! BangBench(processes,messagess)
// println("System configuration: \n"+masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, processes, nodes))  
}
