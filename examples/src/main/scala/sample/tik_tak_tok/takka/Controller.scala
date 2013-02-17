package sample.tik_tak_tok.takka

import takka.actor._

final class Controller(model:ActorRef[Controller2ModelMessage], viewer:ActorRef[Controller2ViewerMessage]) extends TypedActor[ControllerMessage] {
  def typedReceive = {
    case _ =>
  }
  
  override def preStart() = {
//    model ! ModelsetController(typedSelf) //pass?
//    viewer ! ViewersetController(typedSelf) //pass?
    model ! ModelsetController(typedSelf.publishAs[Model2ControllerMessage])
    viewer ! ViewersetController(typedSelf.publishAs[Viewer2ControllerMessage])
  }
}