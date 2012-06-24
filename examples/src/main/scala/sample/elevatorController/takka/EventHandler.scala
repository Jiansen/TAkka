package sample.elevatorController.takka

trait EventHandler {
  def init(arg:Any):EventHandler
  
  def handle_event(event:Event)
 
}

//Events to be handlered by EventHandler
sealed trait Event
case class ResetEvent(eNo:Int, state:ElevatorState, floor:Floor) extends Event
case class OpenEvent(ENo:Int) extends Event
case class CloseEvent(ENo:Int) extends Event
case class MoveEvent(ENo:Int, Dir:Direction) extends Event
case class StoppingEvent(ENo:Int) extends Event
case class ApproachingEvent(ENo:Int, floor:Floor) extends Event
case class StoppedEvent(ENo:Int, floor:Floor) extends Event
case class PassingEvent(ENo:Int, floor:Floor) extends Event
case class EButtonPressedEvent(ENo:Int, floor:Floor) extends Event
case class FButtonPressedEvent(floor:Floor) extends Event
case class ControllerStartedEvent(ENo:Int, EPid:takka.actor.ActorRef[ElevatorMessage]) extends Event

case class RepairEvent(eNo:Int) extends Event
    