package sample.tik_tak_tok.takka

import takka.actor._

final class Controller(model:ActorRef[Controller2ModelMessage], view:ActorRef[Controller2ViewMessage]) extends TypedActor[ControllerMessage] {
  def typedReceive = {
    case ButtonClickedAt(row, col) =>
      model ! MoveAt(row, col)
    case GridNotEmpty(row, col) =>
      view ! DisplyError("grid "+row+" , "+col+" is not empty")
    case PlayedCross(row, col) =>
      view ! DrawCross(row, col)
    case PlayedO(row, col) =>
      view ! DrawO(row:Int, col:Int)
    case NextMove(move) =>
      view ! DisplayNextMove(move)
    case Winner(move) =>
      view ! AnnounceWinner(move)
  }
  
  override def preStart() = {
//    model ! ModelsetController(typedSelf) //pass
//    view ! ViewSetController(typedSelf) //pass
    model ! ModelSetController(typedSelf.publishAs[Model2ControllerMessage])
    view ! ViewSetController(typedSelf.publishAs[View2ControllerMessage])
  }
}