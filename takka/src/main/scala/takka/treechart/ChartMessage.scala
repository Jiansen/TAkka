package takka.treechart

import java.util.Date
import scala.collection.mutable._

import scala.concurrent.duration._
import takka.actor.ActorRef
import akka.actor.ActorPath

//private [takka] sealed trait ChaosMessage
//private [takka] case class ChaosException(e:Exception) extends ChaosMessage
//private [takka] case object ChaosNonTerminate extends ChaosMessage


case class ChartTreeRequest(id:Date, master:ActorRef[ChartTreeResponse])

sealed trait ChartRecorderMessage
case class ChartTreeResponse(id:Date, reportorPath:ActorPath, childrenPath:List[ActorPath]) extends ChartRecorderMessage
case class ReportTo(drawer:ActorRef[Map[Date, TreeSet[NodeRecord]]]) extends ChartRecorderMessage

case class NodeRecord(receiveTime:Date, node:ActorPath, childrenPath:List[ActorPath]) 