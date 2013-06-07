package takka.supervisionview

import scala.concurrent.duration._
import takka.actor.{TypedActor, ActorSystem, ActorRef, Props}
import akka.actor.ActorPath
import com.typesafe.config.Config

import scala.collection.mutable._

import java.util.Date

object ViewMaster{
  def apply(name:String, config: Config, topnodes:List[ActorRef[_]], interval:FiniteDuration):ViewMaster = {
    new ViewMaster(name, config, topnodes, interval)
  }
  
  def apply(name:String, topnodes:List[ActorRef[_]], interval:FiniteDuration):ViewMaster = {
    new ViewMaster(name, null, topnodes, interval)
  }
  
  def apply(topnodes:List[ActorRef[_]], interval:FiniteDuration):ViewMaster = {
    new ViewMaster("default", null, topnodes, interval)
  }
}

private[takka] class ViewMaster(name:String, config: Config, topnodes:List[ActorRef[_]], interval:FiniteDuration) {
  private val system = if(config == null){
    ActorSystem(name);
  }else{
    ActorSystem(name, config);    
  }

  import system.dispatcher
  
  private val recorder = system.actorOf(Props[SupervisionViewMessage, ViewRecorder], "recorder")
  
  private def newID():Long = {
    System.currentTimeMillis()
  }
    
  def start = {
    for(node <- topnodes){
      system.system.scheduler.schedule(0 milliseconds,
    		  interval,
    		  new Runnable {
    	  		def run() {
//    	  		  println("sending request to "+node);
    	  			node.untypedRef ! SupervisionViewRequest(new java.util.Date(System.currentTimeMillis()), recorder);
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
    recorder ! ReportViewTo(drawer)
  }
}

// send message to nodes
// send responses to visualiser
// invoke visualiser
private [supervisionview] class ViewRecorder extends TypedActor[SupervisionViewMessage]{    
  val record : Map[Date, TreeSet[NodeRecord]] = HashMap()
  
  def typedReceive = {
    case SupervisionViewResponse(id:Date, reportorPath:ActorPath, childrenPath:List[ActorPath]) =>{
      val node = NodeRecord(new java.util.Date(System.currentTimeMillis()), reportorPath, childrenPath)
      if(record.contains(id)){
        record(id).add(node)
      }else{
        val newSet = new TreeSet()(ActorPathOrdering)
    	record += ((id, newSet+=node))
      }
//      println("id: "+id +" record:"+record);
    }
    case ReportViewTo(drawer) =>{
      drawer ! record
    }
      
  }
}

object ActorPathOrdering extends Ordering[NodeRecord] {
  def compare(a:NodeRecord, b:NodeRecord) = a.node compareTo b.node
}
