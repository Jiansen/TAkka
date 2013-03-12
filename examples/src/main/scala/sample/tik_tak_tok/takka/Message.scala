package sample.tik_tak_tok.takka

import takka.actor._
//sealed trait Message

sealed trait ControllerMessage
sealed trait View2ControllerMessage extends ControllerMessage
final case class ButtonClickedAt(row:Int, col:Int) extends View2ControllerMessage

sealed trait Model2ControllerMessage extends ControllerMessage
final case class GridNotEmpty(row:Int, col:Int) extends Model2ControllerMessage
final case class PlayedCross(row:Int, col:Int) extends Model2ControllerMessage
final case class PlayedO(row:Int, col:Int) extends Model2ControllerMessage
final case class NextMove(move:Move) extends Model2ControllerMessage
final case class Winner(move:Move) extends Model2ControllerMessage

sealed trait Controller2ViewMessage
final case class DisplyError(err:String) extends Controller2ViewMessage
final case class DrawCross(row:Int, col:Int) extends Controller2ViewMessage
final case class DrawO(row:Int, col:Int) extends Controller2ViewMessage
final case class DisplayNextMove(move:Move) extends Controller2ViewMessage
final case class AnnounceWinner(winner:Move) extends Controller2ViewMessage

sealed trait Controller2ModelMessage
final case class MoveAt(row:Int, col:Int) extends Controller2ModelMessage

final case class ModelSetController(controller:ActorRef[Model2ControllerMessage]) extends Controller2ModelMessage
final case class ViewSetController(controller:ActorRef[View2ControllerMessage]) extends Controller2ViewMessage


sealed trait Move
final case object X extends Move
final case object O extends Move