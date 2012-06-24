package sample.elevatorController.takka

import takka.actor._
import akka.util.duration._
import akka.dispatch.{Future, Await}
import scala.concurrent.ops._
//import takka.pattern.ask
/*
%%%----------------------------------------------------------------------
%%% 
%%% The elevator is represented by an FSM with four states:
%%%  open:     Standing at a floor with the doors open
%%%  closed:   Standing at a floor with the doors closed
%%%  moving:   Moving
%%%  stopping: Moving, but will stop at the next floor
%%%
%%% In addition to the state, the FSM has information about its number and
%%% a floor. The floor is the current floor if the elevator is standing
%%% still, otherwise the last floor it passed.
%%%
%%% When the FSM starts, it's in the state uninitialized, where it only reacts
%%% to the event {reset, Floor}, which causes it to enter the state closed.
%%%
%%% Apart from this, the states, events and corresponding actions are:
%%%
%%%
%%%      State   |     open     |    closed    |    moving     |   stopping
%%% Event        |              |              |               |
%%% -------------+--------------+--------------+---------------+--------------
%%% open         |      N/A     | Open doors   |      N/A      |      N/A
%%%              |              | -> open      |               |
%%% -------------+--------------+--------------+---------------+--------------
%%% close        | Close doors  |      N/A     |      N/A      |      N/A
%%%              | Inform sched |              |               |
%%%              | -> closed    |              |               |
%%% -------------+--------------+--------------+---------------+--------------
%%% {move, Dir}  |      N/A     | Start moving |      N/A      |      N/A
%%%              |              | -> moving    |               |
%%% -------------+--------------+--------------+---------------+--------------
%%% {approaching,|      N/A     |      N/A     | Inform sched  |      N/A
%%%  Floor}      |              |              | -> moving     |
%%%              |              |              | -> stopping   |
%%% -------------+--------------+--------------+---------------+--------------
%%% {at, Floor}  |      N/A     |      N/A     | Update floor, | Update floor,
%%%              |              |              | inform sched  | inform sched
%%%              |              |              | -> moving     | -> open
%%%
%%% (sched = scheduler)
%%%
%%% There is also a synchronous event to retrieve the state of an
%%% elevator, get_state, which is handled in all states. It returns
%%% the tuple {State, Floor}.
%%%
%%%----------------------------------------------------------------------
*/

class ElevatorClass(ENo:Int) extends Actor[ElevatorMessage] with FSM[ElevatorState, Floor, ElevatorMessage] {
  startWith(Uninitialized, UnknownFloor, None)

  when(_:ElevatorState) {
    case Event(ERepair, _) =>
      println("Hello, I'm elevator ")//+ ENo)
      //this.stop
      stay
  }
  
  when(Uninitialized) {
    case Event(Reset(state, floor), UnknownFloor) =>
      scheduler.set_controller(ENo, typedSelf)
      sys_event.initialized(ENo, state, floor)
      goto(state) using floor
    case Event(getState, floor) => {
      sender ! (ENo, Uninitialized, floor)
      stay
    }    
//    case Event(_other, date) =>
//      goto(Uninitialized) using date
  }
  
  when(DoorOpen) {
    case Event(EClose, floor) => 
      sys_event.close(ENo)
      scheduler.closed(ENo, floor)
      goto(DoorClosed) using floor
    case Event(getState, floor) =>
      sender ! (ENo, DoorOpen, floor)
      stay

  } 
    
  when(DoorClosed) {
    case Event(EOpen, floor) => {
      sys_event.open(ENo)
      //timer:apply_after(5*list_to_integer(os:getenv("eqctimeout")), elevator, close, [self()]),
      spawn{
        Thread.sleep(5 * config.eqctimeout)
        elevator.close(typedSelf)
      }            
      goto(DoorOpen) using floor
    }
    case Event(EMove(dir), floor) => {
      sys_event.move(ENo, dir)
      goto(Moving) using floor
    }
    case Event(getState, floor) =>
      sender ! (ENo, DoorClosed, floor)
      stay
  }
  
  when(Moving) {
    case Event(EApproaching(newFloor), floor) =>
      sys_event.approaching(ENo, newFloor)
      scheduler.approaching(ENo, newFloor) match{
        case Stop => 
          sys_event.stopping(ENo)
          goto(Stopping) using floor
        case Continue =>
          goto(Moving) using floor
        case _Other =>
          sys_event.stopping(ENo)
          goto(Stopping) using floor
      }
    case Event(EAt(newfloor) , floor) =>
      scheduler.passing(ENo, newfloor)
      goto(Moving) using newfloor
    case Event(getState, floor) =>
      sender ! (ENo, Moving, floor)
      stay
  }

  when(Stopping) {
    case Event(EAt(newFloor), floor) =>
      sys_event.stopped_at(ENo, newFloor)
      sys_event.open(ENo)
      scheduler.open(ENo, newFloor)
      //timer:apply_after(5*list_to_integer(os:getenv("eqctimeout")), elevator, close, [self()]),
      spawn{
        Thread.sleep(5 * config.eqctimeout)
        elevator.close(typedSelf)
      } 
      goto(DoorOpen) using newFloor
    case Event(getState, floor) =>
      sender ! (ENo, Stopping, floor)
      stay
//    case Event(Maintenance, floor) =>
//      throw new Exception("Maintenancing Elevator "+ENo)
  }  
  
  override def preStart() = {
    println("Starting "+typedSelf)
    sys_event.controller_started(ENo, typedSelf)    
  }
  
  override def preRestart(cause: Throwable, msg: Option[Any]) {
    println("Restarting "+typedSelf)
  }
  
  initialize
}

object elevator {
  def start_link(eNo:Int):Actor[ElevatorMessage] = {
    new ElevatorClass(eNo)
  }
  
  def repair(elev:ActorRef[ElevatorMessage]) = {
    elev ! ERepair
  }
  
  def reset(elev:ActorRef[ElevatorMessage], state:ElevatorState, floor:Floor) {
    elev ! Reset(state, floor)
  }
  
  def move(elev:ActorRef[ElevatorMessage], dir:Direction) {
    elev ! EMove(dir)
  }

  def stop(elev:ActorRef[ElevatorMessage]) { //TODO
    //    gen_fsm:send_event(Elev, stop).
  }

  def open(elev:ActorRef[ElevatorMessage]) {
    elev ! EOpen
  }

  def close(elev:ActorRef[ElevatorMessage]) {
    elev ! EClose
  }
  
  def approaching(elev:ActorRef[ElevatorMessage], floor:Floor) {
    elev ! EApproaching(floor)
  }

  def atFloor(elev:ActorRef[ElevatorMessage], floor:Floor) {
    elev ! EAt(floor)
  }

  def get_state(elev:ActorRef[ElevatorMessage]):(Int, ElevatorState, Floor) = {
//    val res = ask(elev, getState)(1 second).mapTo[(Int, ElevatorState, Floor)]
    implicit val timeout = akka.util.Timeout(1 second)
    val res = elev.?[(Int, ElevatorState, Floor)](getState)
    Await.result(res, 1 second)
  }
  
  //TODO: code_change(_OldVsn, State, Data, _Extra)
  //TODO: terminate(_Reason, _State, {ENo, _Floor})
  
}