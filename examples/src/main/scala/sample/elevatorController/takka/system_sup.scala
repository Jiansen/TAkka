package sample.elevatorController.takka

import takka.actor._

class system_sup(nElevs:Int, eventHandlers:List[EventHandler]) extends Actor[Unit] {
  private var sch:ActorRef[SchedulerMessage] = _
  private var ele_sup:ActorRef[Unit] = _
  
  /*
  def receive = {
    case child_prop:Props => context.actorOf(child_prop)
  }
  */
  def typedReceive = {case _ => }
  
  override def preStart() = {
    typedContext.actorOf(Props[SchedulerMessage](scheduler.start_link()), "scheduler")
    sys_event.start_link(eventHandlers)
    typedContext.actorOf(Props[Unit](new elev_sup(nElevs)), "elev_sup")
  }

}