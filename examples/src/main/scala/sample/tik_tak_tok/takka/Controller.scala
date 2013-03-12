package sample.tik_tak_tok.takka

import takka.actor._

final class Controller(model:ActorRef[Controller2ModelMessage], viewer:ActorRef[Controller2ViewerMessage]) extends TypedActor[ControllerMessage] {
  def typedReceive = {
    case ButtonClickedAt(row, col) =>
      model ! MoveAt(row, col)
    case GridNotEmpty(row, col) =>
      viewer ! DisplyError("grid "+row+" , "+col+" is not empty")
    case PlayedCross(row, col) =>
      viewer ! DrawCross(row, col)
    case PlayedO(row, col) =>
      viewer ! DrawO(row:Int, col:Int)
    case NextMove(move) =>
      viewer ! DisplayNextMove(move)
    case Winner(move) =>
      viewer ! AnnounceWinner(move)
  }
  
  override def preStart() = {
//    model ! ModelsetController(typedSelf) //pass?
//    viewer ! ViewersetController(typedSelf) //pass?
    model ! ModelsetController(typedSelf.publishAs[Model2ControllerMessage])
    viewer ! ViewersetController(typedSelf.publishAs[Viewer2ControllerMessage])
  }
}