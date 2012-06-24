package sample.elevatorController.akka

import akka.actor._

class sim_sup(iFloor:Int, nFloor:Int, nElevs:Int) extends Actor {
  def receive = {
    case _ =>
  }

  override def preStart() = {
    //context.actorOf(Props(new g_sup()), ("g_sup"))
    context.actorOf(Props(new system_sup(nElevs, List(display.init((iFloor, nFloor, nElevs))))), ("system_sup"))
  }
}