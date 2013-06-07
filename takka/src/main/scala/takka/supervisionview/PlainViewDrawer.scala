package takka.supervisionview

import scala.concurrent.duration._
import takka.actor.{TypedActor, ActorSystem, ActorRef, Props}
import akka.actor.ActorPath

import scala.collection.mutable._

import java.util.Date

class PlainViewDrawer extends TypedActor[Map[Date, TreeSet[NodeRecord]]]{
  
  def typedReceive = {
    case map =>
      for( (date, nodes) <- map.iterator){
         println(date+": "+ nodes)
         println("======")
      }
  }
  
}