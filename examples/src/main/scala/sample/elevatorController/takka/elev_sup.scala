package sample.elevatorController.takka

import takka.actor._
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy._
import akka.util.duration._

class elev_sup(nElevs:Int) extends Actor[Unit] {
/*
  def receive = {
    case child_prop:Props => context.actorOf(child_prop)
  }
*/
  def typedReceive = { case _ => }
  override def preStart() = {
    for(ENo <- 1 to nElevs) { 
      typedContext.actorOf(Props[ElevatorMessage](elevator.start_link(ENo)), ("Elevator_"+ENo)) 
    }
  }

  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
    case _: Exception                => 
      Restart
  }
}