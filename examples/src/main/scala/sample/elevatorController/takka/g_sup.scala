package sample.elevatorController.takka

import takka.actor._

sealed trait GSupMessage
case class GElevatorProps(props:Props[ElevatorMessage]) extends GSupMessage with SynMessage[ActorRef[ElevatorMessage]]

class g_sup() extends Actor[GSupMessage]{
  def typedReceive = {
    case GElevatorProps(child_prop) => 
      sender ! typedContext.actorOf(child_prop)
  }
}