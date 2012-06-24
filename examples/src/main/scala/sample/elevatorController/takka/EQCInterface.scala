package sample.elevatorController.takka

sealed trait EQCEvent
case class OpenEQCEvent(elev:Int, floor:Floor) extends EQCEvent
case class FloorEQCEvent(floor:Floor) extends EQCEvent
case class ElevatorEQCEvent(eNo:Int,floor:Floor) extends EQCEvent

