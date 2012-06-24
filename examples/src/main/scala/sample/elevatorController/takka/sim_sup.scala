package sample.elevatorController.takka

import takka.actor._

class sim_sup(iFloor:Int, nFloor:Int, nElevs:Int) extends Actor[Unit] {
  def typedReceive = {
    case _ =>
  }

  override def preStart() = {
    //context.actorOf(Props(new g_sup()), ("g_sup"))
    typedContext.actorOf(Props[Unit](new system_sup(nElevs, List(display.init((iFloor, nFloor, nElevs))))), ("system_sup"))
    
//    println(self.path)
//    println(self.isTerminated)
  }
}