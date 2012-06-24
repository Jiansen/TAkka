package sample.elevatorController.akka

import akka.actor._

class elev_sup(nElevs:Int) extends Actor {

  def receive = {
    case child_prop:Props => context.actorOf(child_prop)
  }

  override def preStart() = {
    for(ENo <- 1 to nElevs) { 
      context.actorOf(Props(elevator.start_link(ENo)), ("Elevator_"+ENo)) 
    }
  }

}