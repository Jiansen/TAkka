package sample.elevatorController.takka


import scala.concurrent.ops._
import takka.actor._
import akka.event._

/*
  Different from Erlang version,
  the util object in this module is not an actor.
  The Scala implementation will be stopped when
  the GUI is closed.
  There is no need to send a "stop" message to util object.
 */

object util {
  var system:ActorSystem = ActorSystem("util")
  
  def start(IFloor:Int, NFloors:Int, NElevs:Int) = spawn {
    top_proc(IFloor, NFloors, NElevs, Nil)
  }
  
  def start_eqc(IFloor:Int, NFloors:Int, NElevs:Int) = spawn {
    top_proc(IFloor, NFloors, NElevs, List(handler_eqc.init((IFloor, NFloors, NElevs))))
  }

/*
%%----------------------------------------------------------------------
%% start_trace(IFloor, NFloors, NElevs)
%%  Starts the system without supervision (except for the graphics
%%  portion). Installs the tracer event handler.
%%----------------------------------------------------------------------
*/
  def start_trace(IFloor:Int, NFloors:Int, NElevs:Int) = spawn{
    top_proc(IFloor, NFloors, NElevs, List(tracer.init(Nil)))
  }
  
  def start_sup(IFloor:Int, NFloors:Int, NElevs:Int) = {
    top_proc_sup(IFloor, NFloors, NElevs)
  }
  
  def stop() = {
    sys.exit(0)
  }
  
  def stop_sup(supActor:ActorRef[Unit]) = {
    display.close()
    sys_event.clear()
    //scheduler.stop()
    system.system.stop(supActor.untypedRef)
    //Thread.sleep(1000)
    system.shutdown()
    system = ActorSystem("util")    
  }
  
  def top_proc(IFloor:Int, NFloors:Int, NElevs:Int, Handlers:List[EventHandler]){
    val schedulerRef = system.actorOf(Props(scheduler.start_link()), "scheduler")    
    sys_event.start_link(display.init((IFloor, NFloors, NElevs))::Handlers)
    for(ENo <- 1 to NElevs) { ActorSystem("util").actorOf(Props(elevator.start_link(ENo)), ("Elevator_"+ENo)) }
    Thread.sleep(500)
  }

  def top_proc_sup(IFloor:Int, NFloors:Int, NElevs:Int):ActorRef[Unit] = {
    system.actorOf(Props[Unit](new sim_sup(IFloor, NFloors, NElevs)), "sim_sup")
  }

}