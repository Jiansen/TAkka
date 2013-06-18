package takka.supervisionview

import java.util.Date
import scala.collection.mutable._

import scala.concurrent.duration._
import takka.actor.ActorRef
import akka.actor.ActorPath

//private [takka] sealed trait ChaosMessage
//private [takka] case class ChaosException(e:Exception) extends ChaosMessage
//private [takka] case object ChaosNonTerminate extends ChaosMessage


case class SupervisionViewRequest(date:Date, master:ActorRef[SupervisionViewResponse])

sealed trait SupervisionViewMessage
case class SupervisionViewResponse(date:Date, reportorPath:ActorPath, childrenPath:List[ActorPath]) extends SupervisionViewMessage
case class ReportViewTo(drawer:ActorRef[Map[Date, TreeSet[NodeRecord]]]) extends SupervisionViewMessage

case class NodeRecord(receiveTime:Date, node:ActorPath, childrenPath:List[ActorPath]) 