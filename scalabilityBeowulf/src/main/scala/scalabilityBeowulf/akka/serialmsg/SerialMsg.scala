package scalabilityBeowulf.akka.serialmsg

/*
Short description: Message proxying through a dispatcher

Arguments: P ( the number of receivers ), N ( the  number of messages ), 
L ( the data length )

Function: push

Long description: Spawns P receivers, 1 dispatcher and and P generators. The 
dispatcher forwards the messages that it receives from generators to the 
appropriate receiver. Each receiver receives messages from a specific generator 
and ignores them. Each generator sends N messages to a specific receiver. Each 
message contains a list of integers between 1 and L.
 */
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.remote._

import util.{BenchTimer, BenchCounter}
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._
import language.postfixOps

import akka.chaos._
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy._
import akka.actor.OneForOneStrategy

sealed trait DispatcherMsg
sealed trait ReceiverMsg
sealed trait MasterMsg
sealed trait GeneratorMsg

case class Push(p:Int, n:Int, l:Int) extends MasterMsg
case class RecvDone(recv:ActorRef) extends MasterMsg
case class GeneratorDone(generator:ActorRef, recv:ActorRef) extends DispatcherMsg 
case class GeneratorData(generator:ActorRef, recv:ActorRef, data:List[Int]) extends DispatcherMsg 

case class Do(master:ActorRef) extends GeneratorMsg
case class DoNewN(master:ActorRef, newN:Int) extends GeneratorMsg

case class Done(generator:ActorRef) extends ReceiverMsg
case class Data(generator:ActorRef, data:List[Int]) extends ReceiverMsg
case class SetMaster(master:ActorRef) extends ReceiverMsg

class Receiver extends Actor {
  var master:ActorRef= _
  def receive = {
    case SetMaster(master) =>
      this.master = master
    case Done(_) =>
      master ! RecvDone(self)
    case _ =>
      // discard received message i.e. Data(generator:ActorRef, data:List[Int])
  }
}

class Dispatcher extends Actor {
  def receive = {
    case GeneratorDone(generator, recv) =>
      recv ! Done(generator)
    case GeneratorData(generator, recv, data) =>
      recv ! Data(generator, data)
  }
}

class Generator(recv:ActorRef, disp:ActorRef, n:Int, l:Int) extends Actor {
  val data = (1 to l) toList
  
  def generator_push_loop(recv:ActorRef, disp:ActorRef, n:Int, data:List[Int]):Unit = {    
    if (n == 0) {
      disp ! GeneratorDone(self, recv)
    }else{
      disp ! GeneratorData(self, recv, data)
      generator_push_loop(recv, disp, n-1, data)      
    }
  }
  
  def receive = {
    case Do(master) =>
      generator_push_loop(recv, disp, n, data)
    case DoNewN(master, newN) =>
      generator_push_loop(recv, disp, newN, data)
  }
}

class PushActor extends Actor {
  val timer = new BenchTimer
   val counter = new BenchCounter
  
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
      case e  =>
        Resume    
  }
  
  def receive = {
    case Push(p, n, l) => {
      counter.set(p)
      
      val recvs:List[ActorRef] = (for (i<-1 to p) yield {
        context.actorOf(Props[Receiver], ProcessNamePrefix+i)
      }).toList
      for(recv <- recvs) {
        recv ! SetMaster(self)
      }

      val disp = context.actorOf(Props[Dispatcher])
      
      val gens = (for (recv<-recvs) yield {
        context.actorOf(Props[Generator].withCreator(new Generator(recv, disp, n, l)))
      }).toList
      
      if(util.Configuration.EnableChaos){
        import akka.chaos.ChaosMode._
        val chaos = ChaosMonkey(gens)
        chaos.setMode(Kill)
//        chaos.enableDebug
        chaos.start(1 second)
      }
      
      this.timer.start
      for (gen <- gens) {gen ! Do(self)}
    }
    case RecvDone(recv) =>
      counter.decrement
      if(util.Configuration.TraceProgress){
          println("Remaining processes: "+counter.get)
        }
      if (counter.isZero) {
        this.timer.finish
        this.timer.report
        sys.exit()
      }
  }
}

object SerialMsgBench extends App {
  private val nodes:Int = args(0).toInt
  private val processes:Int = 128

  private val system = ActorSystem("SerialMsgSystem", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, processes, nodes))
  val testActor = system.actorOf(Props[PushActor], ProcessPathPrefix)
  testActor ! Push(processes, 100, 200)
}
