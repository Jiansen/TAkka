package sample.tik_tak_tok.takka

import takka.actor._
//sealed trait Message

sealed trait ControllerMessage
sealed trait Controller2ViewerMessage
sealed trait Viewer2ControllerMessage extends ControllerMessage
sealed trait Controller2ModelMessage
sealed trait Model2ControllerMessage extends ControllerMessage


final case class ModelsetController(controller:ActorRef[Model2ControllerMessage]) extends Controller2ModelMessage
final case class ViewersetController(controller:ActorRef[Viewer2ControllerMessage]) extends Controller2ViewerMessage

final case class ModelMove(row:Int, col:Int, move:Move) extends Controller2ModelMessage


sealed trait Move
final case object X extends Move
final case object O extends Move