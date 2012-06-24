package sample.elevatorController.akka

object sys_event {
/*
-export([start_link/1, add_handler/2]).
-export([initialized/3, open/1, close/1, move/2, stopping/1,
	 approaching/2, stopped_at/2, passing/2,
	 f_button_pressed/1, e_button_pressed/2,
	 controller_started/2]).
 */
  private var handlers:List[EventHandler] = Nil
  
  def start_link(e_handlers:List[EventHandler]) = {//TODO
//    Ret = gen_event:start_link({local, sys_event}),
    for (eh <- e_handlers) {handlers = eh::handlers}
  }
  
  //An elevator has been initialized
  def initialized(ENo:Int, State:ElevatorState, Floor:Floor) {
    notify(ResetEvent(ENo, State, Floor))
  }
  
  //The doors of an elevator have opened.
  def open(ENo:Int) {  notify(OpenEvent(ENo))  }
  
  //The doors of an elevator have opened.
  def close(ENo:Int) {  notify(CloseEvent(ENo)) }
  
  //An elevator has started moving in the direction Dir
  def move(ENo:Int, Dir:Direction) { notify(MoveEvent(ENo, Dir)) }
  
  //An elevator will stop at the next floor.
  def stopping(ENo:Int) { notify(StoppingEvent(ENo)) }

  //An elevator is nearing a floor.
  def approaching(ENo:Int, floor:Floor) { notify(ApproachingEvent(ENo, floor)) }

  //An elevator has stopped at a floor.
  def stopped_at(ENo:Int, floor:Floor) { notify(StoppedEvent(ENo, floor)) }
  
  //An elevator is passing a floor.
  def passing(ENo:Int, floor:Floor) { notify(PassingEvent(ENo, floor)) }
  
  //A floor button in an elevator has been pressed.
  def e_button_pressed(ENo:Int, floor:Floor) { notify(EButtonPressedEvent(ENo, floor)) }
  
  //A call button has been pressed on a floor.
  def f_button_pressed(floor:Floor) {notify(FButtonPressedEvent(floor))}

  //An elevator control process has been started (or restarted)
  def controller_started(ENo:Int, EPid:akka.actor.ActorRef) {notify(ControllerStartedEvent(ENo, EPid)) }
    
  def notify(event:Event) {
    for (eh <- handlers) {eh.handle_event(event)}
  }
  
  def addHandler(h:EventHandler) {
    if(!handlers.contains(h))
      handlers = h::handlers
  }
  
  def removeHandler(h:EventHandler) {
    handlers = handlers.filter(hd => hd == h)
  }
  
  def clear() {
    handlers = Nil
  }
  
}