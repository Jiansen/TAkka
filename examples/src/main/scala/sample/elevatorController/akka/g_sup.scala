package sample.elevatorController.akka

import akka.actor._


class g_sup() extends Actor{
  def receive = {
    case child_prop:Props => 
      sender ! context.actorOf(child_prop)
  }
}