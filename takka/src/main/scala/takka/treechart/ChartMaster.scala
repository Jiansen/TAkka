package takka.treechart

import scala.concurrent.duration._
import takka.actor.{TypedActor, ActorSystem, ActorRef, Props}
import akka.actor.ActorPath

import scala.collection.mutable._

import java.util.Date


class ChartTreeMaster(val topnodes:List[ActorRef[_]], interval:FiniteDuration) {

  private val system = ActorSystem(); // name?
  
  import system.dispatcher
  
  private val recorder = system.actorOf(Props[ChartRecorderMessage, ChartRecorder])
  
  private def newID():Long = {
    System.currentTimeMillis()
  }
    
  def start = {
    for(node <- topnodes){
      system.system.scheduler.schedule(0 milliseconds,
    		  interval,
    		  new Runnable {
    	  		def run() {
    	  			node.untypedRef ! ChartTreeRequest(new java.util.Date(System.currentTimeMillis()), recorder);
    	  		}
      		  })
    }
  }
  
  def stop = {
    system.stop(recorder)
//    println(recorder.path)
//    println(recorder.isTerminated)
  }
  
  def reportTo(drawer:ActorRef[Map[Date, TreeSet[NodeRecord]]]) = {
    recorder ! ReportTo(drawer)
  }
}

// send message to nodes
// send responses to visualiser
// invoke visualiser
private [treechart] class ChartRecorder extends TypedActor[ChartRecorderMessage]{    
  val record : Map[Date, TreeSet[NodeRecord]] = HashMap()
  
  def typedReceive = {
    case ChartTreeResponse(id:Date, reportorPath:ActorPath, childrenPath:List[ActorPath]) =>{
      val node = NodeRecord(new java.util.Date(System.currentTimeMillis()), reportorPath, childrenPath)
      if(record.contains(id)){
        record(id).add(node)
      }else{
        val newSet = new TreeSet()(ActorPathOrdering)
    	record += ((id, newSet+=node))
      }
      println("id: "+id +" record:"+record);
    }
    case ReportTo(drawer) =>{
      drawer ! record
    }
      
  }
}

object ActorPathOrdering extends Ordering[NodeRecord] {
  def compare(a:NodeRecord, b:NodeRecord) = a.node compareTo b.node
}
