package takka.treechart

import scala.concurrent.duration._
import takka.actor.{TypedActor, ActorSystem, ActorRef, Props}
import akka.actor.ActorPath

import scala.collection.mutable._

import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;

object TreeChartExample extends App{

  val system = ActorSystem("TreeChartExample")
  
  val rootRef = system.actorOf(Props[Unit, RandomActor], "root")
  
  val chart = new ChartTreeMaster(List(rootRef), 2 seconds)
  
  val drawer = system.actorOf(Props[Map[Date, TreeSet[NodeRecord]], PlainChartDrawer])
  
  
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
 * The actor randomly change
 * 
 * 
 * During the initialization stage, the actor has 50% chance
 * to create two children of the same type, has 50% chance not
 * to create any child.
 * 
 */ 
class RandomActor extends TypedActor[Unit]{
  
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
      this.changeState()
  }
  
  
  private def changeState():Unit = {
    ran.nextInt(2) match {  // 0, 1, 2
      case 0 => 
        state = "LIVE"
      case 1 =>
        state = "BLOCKED"
      case 2 => 
        state = "DEAD"
    }
    
    
    state match {
      case "LIVE" =>
      case "BLOCKED" =>
        Thread.sleep(10000) // in ms
      case "DEAD" =>
        self ! akka.actor.PoisonPill
    }

    val delay = 3000 // in ms     
    val timer = new Timer();
    timer.schedule( new TimerTask(){
      def run() { 
    	  changeState()
      }
    }, delay);
  } 
  
  
}