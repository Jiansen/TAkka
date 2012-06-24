package sample.elevatorController.takka

import takka.actor._

sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object NoDir extends Direction

sealed trait ElevatorState
case object Uninitialized extends ElevatorState
case object DoorOpen extends ElevatorState
case object DoorClosed extends ElevatorState
case object Moving extends ElevatorState
case object Stopping extends ElevatorState

sealed trait ElevatorMessage
case object EOpen extends ElevatorMessage
case object EClose extends ElevatorMessage
case class EMove(dir:Direction) extends ElevatorMessage
case class EApproaching(floor:Floor) extends ElevatorMessage
case class EAt(floor:Floor) extends ElevatorMessage
case class Reset(state:ElevatorState, floor:Floor) extends ElevatorMessage
case object getState extends ElevatorMessage with SynMessage[(Int, ElevatorState, Floor)]
//graphic elevator
case class Step(dir:Direction) extends ElevatorMessage
case object StopMessage extends ElevatorMessage
case class EPid(epid:ActorRef[ElevatorMessage]) extends ElevatorMessage

case class GEStateData (//StateDate for Graphic Elevator
    pos:Int, elevG:javax.swing.JPanel, elevator:ActorRef[ElevatorMessage], dir:Direction, floors:List[(Floor, Int)]
    )
case object ERepair extends ElevatorMessage 

case class Floor(f:Int)
case object UnknownFloor extends Floor(-1)