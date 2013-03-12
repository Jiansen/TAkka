package sample.tik_tak_tok.takka

import takka.actor._
//sealed trait Message

sealed trait ControllerMessage
sealed trait Viewer2ControllerMessage extends ControllerMessage
final case class ButtonClickedAt(row:Int, col:Int) extends Viewer2ControllerMessage

sealed trait Model2ControllerMessage extends ControllerMessage
final case class GridNotEmpty(row:Int, col:Int) extends Model2ControllerMessage
final case class PlayedCross(row:Int, col:Int) extends Model2ControllerMessage
final case class PlayedO(row:Int, col:Int) extends Model2ControllerMessage
final case class NextMove(move:Move) extends Model2ControllerMessage
final case class Winner(move:Move) extends Model2ControllerMessage

sealed trait Controller2ViewerMessage
final case class DisplyError(err:String) extends Controller2ViewerMessage
final case class DrawCross(row:Int, col:Int) extends Controller2ViewerMessage
final case class DrawO(row:Int, col:Int) extends Controller2ViewerMessage
final case class DisplayNextMove(move:Move) extends Controller2ViewerMessage
final case class AnnounceWinner(winner:Move) extends Controller2ViewerMessage

sealed trait Controller2ModelMessage
final case class MoveAt(row:Int, col:Int) extends Controller2ModelMessage

final case class ModelsetController(controller:ActorRef[Model2ControllerMessage]) extends Controller2ModelMessage
final case class ViewersetController(controller:ActorRef[Viewer2ControllerMessage]) extends Controller2ViewerMessage


sealed trait Move
final case object X extends Move
final case object O extends Move