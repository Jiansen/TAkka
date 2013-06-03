package scalabilityBeowulf.takka.treechart

import takka.actor.{TypedActor, ActorRef, ActorSystem, Props}
import takka.treechart._
import akka.remote._
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;

import akka.actor.SupervisorStrategy._
import akka.actor.OneForOneStrategy
import scala.collection.mutable._

import com.typesafe.config.Config

object ParallelTreeDemo extends App {
  private val nodes:Int = args(0).toInt
  private val processes:Int = 32
  private val system = ActorSystem("ParallelTreeDemo", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, processes, nodes))
  val root = system.actorOf(Props[Start, RootActor], ProcessPathPrefix)

  val config:Config = {
    ConfigFactory.parseString(
    """
        include "common"
    akka {
      actor {
        provider = "akka.remote.RemoteActorRefProvider"
      }
      remote {
        startup-timeout = 200 s
//        enabled-transports = ["akka.remote.netty.tcp"]
        transport = "akka.remote.netty.NettyRemoteTransport"
//        netty.tcp{
        netty {
          hostname = "137.195.143.132"
          port = 2555
        }
      }
    } 
    """
    )
  }
  
  val chart = ChartTreeMaster("chart", config, List(root), 2 seconds)
  val drawer = system.actorOf(Props[Map[Date, TreeSet[NodeRecord]], PlainChartDrawer])
  
  root ! Start(processes)
  
  chart.start
    // report the result after a delay
  val delay = 20000;// in ms 
  val timer = new Timer();

  timer.schedule( new TimerTask(){
    def run() { 
       chart.reportTo(drawer)
       chart.stop
    }
  }, delay);
  
}

object ChaosBangBench extends App{
  import takka.chaos.ChaosMode._
  import takka.chaos._
  
  private val nodes:Int = args(0).toInt
  private val processes:Int = 32
  private val system = ActorSystem("ParallelTreeChaos", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, processes, nodes))
  
  val vitims = (for (i <- 1 to processes) yield {
    system.actorFor[Unit](WorkerProcessAddress(i, nodes))
  }).toList
  
  val chaos = ChaosMonkey(vitims)
  chaos.setMode(Kill)
  chaos.enableDebug
  chaos.start(1 second)
}



case class NodeStart

class NodeRoot extends TypedActor[NodeStart]{  
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
      case e  =>
//        println("Error: "+e)
        Restart    
  }
  
  def typedReceive = {
    case NodeStart() =>
      for ( i <- 1 to 3){
        val child = typedContext.actorOf(Props[Unit, ParallelRandomActor], ProcessNamePrefix+i)
      }
  }
}


/*
 * A RandomActor may be in one of the following state at a given time
 * 
 * state = LIVE | BLOCKED | DEAD
 * 
 * In the LIVE state, the actor will reply to message in time
 * In the BLOCKED state, the actor will reply to message in a delay
 * In the DEAD state, the actor will not reply to any message.
 * 
 * 
 * The actor randomly change its state every 5 seconds
 * 
 * 
 * During the initialization stage, the actor has 50% chance
 * to create two children of the same type, has 50% chance not
 * to create any child.
 * 
 */ 
class ParallelRandomActor extends TypedActor[Unit]{
  
  import scala.util.Random
  
  val ran = new Random()
  
  var state = "LIVE"
  
  override def preStart() {
    if (ran.nextBoolean){
      val child1 = typedContext.actorOf(Props[Unit, RandomActor])
    }
    if (ran.nextBoolean){
      val child2 = typedContext.actorOf(Props[Unit, RandomActor])
    }
    
    changeState()
  }
  
  def typedReceive = {
    case _ =>
      println(typedSelf+" received message from " +sender);
//      this.changeState()
  }
  
  
  private def changeState():Unit = {
    ran.nextInt(2) match {  // 0, 1, 2
      case 0 => 
        state = "LIVE"
      case 1 =>
        state = "BLOCKED"
      case 2 => 
//        state = "DEAD"
    }
    
    
    state match {
      case "LIVE" =>
      case "BLOCKED" =>
      	fib(30) // create a block
//      case "DEAD" =>
//        self ! akka.actor.PoisonPill
    }

    val delay = 3000 // in ms     
    val timer = new Timer();
    timer.schedule( new TimerTask(){
      def run() { 
    	  changeState()
      }
    }, delay);
  } 
  
  private def fib(n:Int):Int = {
    if(n<=1) {1}
    else {fib(n-1)+fib(n-2)}
  }
}