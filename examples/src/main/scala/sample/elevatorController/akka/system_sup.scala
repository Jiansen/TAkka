package sample.elevatorController.akka

import akka.actor._

class system_sup(nElevs:Int, eventHandlers:List[EventHandler]) extends Actor {
  private var sch:ActorRef = _
  private var ele_sup:ActorRef = _
  
  def receive = {
    case child_prop:Props => context.actorOf(child_prop)
  }
  
  override def preStart() = {
    context.actorOf(Props(scheduler.start_link()), "scheduler")
    sys_event.start_link(eventHandlers)
    context.actorOf(Props(new elev_sup(nElevs)), "elev_sup")
  }

}