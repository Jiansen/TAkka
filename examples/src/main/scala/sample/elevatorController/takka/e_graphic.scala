package sample.elevatorController.takka

import takka.actor._
import akka.util.duration._
import akka.dispatch.{Future, Await}
import javax.swing.JPanel
import scala.concurrent.ops._
//import pattern.ask
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
    


class GraphicElevator(pos:Int, elevG:(Int,JPanel), floors:List[(Floor, Int)]) extends Actor[ElevatorMessage] with FSM[ElevatorState, GEStateData, ElevatorMessage] {
  startWith(DoorClosed, GEStateData(pos, elevG._2, null, NoDir, floors))

  when(DoorOpen) {
    case Event(EClose, GEStateData(pos, elevG, ePid, NoDir, floors)) => 
      elevG.setBackground(java.awt.Color.black) // TODO: check here
      goto(DoorClosed) using GEStateData(pos, elevG, ePid, NoDir, floors)
    case Event(EPid(epid), GEStateData(pos, elevG, oldPid, dir, floors)) =>
      elevator.reset(epid, DoorOpen, getFloor(pos, dir, floors))
      stay using GEStateData(pos, elevG, epid, dir, floors)
    case Event(ERepair, GEStateData(_, _, ePid, _, _)) =>
      elevator.repair(ePid)
      stay          
  } 
    
  when(DoorClosed) {
    //case Event(EClose, GEStateData(pos, elevG, ePid, NoDir, floors)) => {
    //  throw new Error(GEStateData(pos, elevG, ePid, NoDir, floors).toString())
    //  stay
    //}
    case Event(EOpen, GEStateData(pos, elevG, ePid, NoDir, floors)) => {
      elevG.setBackground(java.awt.Color.cyan) // TODO: check here
      goto(DoorOpen) using GEStateData(pos, elevG, ePid, NoDir, floors)
    }
    case Event(EMove(dir), GEStateData(pos, elevG, ePid, NoDir, floors)) => {
      self ! Step(dir)
      goto(Moving) using GEStateData(pos, elevG, ePid, dir, floors)
    }
    case Event(EPid(epid), GEStateData(pos, elevG, oldPid, dir, floors)) => {
      elevator.reset(epid, DoorClosed, getFloor(pos, dir, floors))
      stay using GEStateData(pos, elevG, epid, dir, floors)
    }
    case Event(ERepair, GEStateData(pos, elevG, ePid, dir, floors)) => {
      elevator.repair(ePid)
      stay using GEStateData(pos, elevG, ePid, dir, floors)
    }
  }
  
  when(Moving) {
    case Event(Step(dir), GEStateData(pos, elevG, ePid, state_dir, floors)) if dir == state_dir =>
      val ndy = dy(dir)
      val newPos = pos + ndy
      val loc = elevG.getLocation()
      elevG.setLocation(loc.getX().toInt,loc.getY().toInt+ndy)
      check_position(newPos, dir, ePid, floors)
      spawn{
            Thread.sleep(config.eqctimeout)
            self ! Step(dir)
      }
      //timer:apply_after(list_to_integer(os:getenv("eqctimeout")), gen_fsm, send_event, [self(), {step, Dir}]),
      stay using GEStateData(newPos, elevG, ePid, dir, floors)
    case Event(StopMessage, GEStateData(pos, elevG, ePid, dir, floors)) =>
      goto (Stopping) using GEStateData(pos, elevG, ePid, dir, floors)
    case Event(EPid(epid), GEStateData(pos, elevG, oldPid, dir, floors)) =>
      elevator.reset(epid, Moving, getFloor(pos, dir, floors))
      stay using GEStateData(pos, elevG, epid, dir, floors)
    case Event(_, GEStateData(_, _, ePid, _, _)) =>
      elevator.repair(ePid)
      stay          
  }
  
  when(Stopping) {
    case Event(Step(dir), GEStateData(pos, elevG, ePid, state_dir, floors)) if dir == state_dir =>
      atFloor(pos, floors) match {
        case false =>
          val ndy = dy(dir)
          val newPos = pos + ndy
          //gs:config(ElevG, {move, {0, Dy}}),
          val loc = elevG.getLocation()
          elevG.setLocation(loc.getX().toInt,loc.getY().toInt+ndy)
          //elevG.getParent().repaint()
          spawn{
            Thread.sleep(config.eqctimeout)
            self ! Step(dir)
          }
	      //timer:apply_after(list_to_integer(os:getenv("eqctimeout")), gen_fsm, send_event, [self(), {step, Dir}]),
          stay using GEStateData(newPos, elevG, ePid, dir, floors)
        case (true, floor:Floor) =>
          elevator.atFloor(ePid, floor)
          goto (DoorClosed) using GEStateData(pos, elevG, ePid, NoDir, floors)
      }
    case Event(EPid(epid), GEStateData(pos, elevG, oldPid, dir, floors)) =>
      elevator.reset(epid, Stopping, getFloor(pos, dir, floors))
      stay using GEStateData(pos, elevG, epid, dir, floors)
    case Event(ERepair, GEStateData(_, _, ePid, _, _)) =>
      elevator.repair(ePid)
      stay          
  }  
  
  initialize
  
  //Internal Functions
  
  //Returns the y-offset to move the graphical elevator with when it's
  //travelling in the direction dir.
  private def dy(dir:Direction):Int = dir match {
    case Up => -10 //  In Erlang, it is 10
    case Down => 10 // In Erlang, it is - 10
    case _ => 0
  }
  
  // Checks whether an elevator at position pos, travelling in the direction
  // dir is approaching a floor. If so, the elevator control process ePid
  // is informed
  private def check_position(pos:Int, dir:Direction, ePid:ActorRef[ElevatorMessage], floors:List[(Floor, Int)]) = {
    floors.find(f => f._2 == pos + 2 * dy(dir)) match {// TODO: may need to change the value
      case Some((floor, _)) => elevator.approaching(ePid, floor)
      case None => check_arrived(pos, ePid, floors)
    }
  }

  //Checks whether an elevator at position Pos is at a floor. If so, the
  //elevator control process EPid is informed
  private def check_arrived(pos:Int, ePid:ActorRef[ElevatorMessage], floors:List[(Floor, Int)]) = {
    atFloor(pos, floors) match {
      case (true, floor:Floor) => elevator.atFloor(ePid, floor)
      case false => Unit
    }
  }
  
  private def atFloor(pos:Int, floors:List[(Floor, Int)]):Any = {//TODO: refine types
    floors.find(f => f._2 == pos) match {// TODO: may need to change the value
      case Some((floor, _)) => (true, floor)
      case None => false
    }    
  }
  
  //Retrieves the last floor passed when the graphical elevator is at Pos,
  // travelling in the direction Dir (or standing still at a floor).
  private def getFloor(pos:Int, dir:Direction, floors:List[(Floor, Int)]):Floor = dir match {
    case NoDir =>
      floors.find(f => f._2 == pos) match {
        case Some((floor, _)) => floor
        case None => throw new Error("e_graphic.scala: code should not reach here")
      }
    case Up => find(1, pos, floors, Int.MaxValue, UnknownFloor)
    case Down => find(-1, pos, floors, Int.MaxValue, UnknownFloor)
  }
  
  private def find(sign:Int, pos:Int, floors:List[(Floor, Int)], min:Int, minFloor:Floor):Floor = floors match {
    case Nil => minFloor
    case (f,y)::floors =>
      if (sign * (y-pos) < 0 ) {find(sign, pos, floors, min, minFloor) }
      else if (sign * (y-pos) < min) {find(sign, pos, floors, sign * (y - pos), minFloor) }
      else find(sign, pos, floors, min, minFloor)
  }
}
    
    
object e_graphic {
  def start_link(pos:Int, elevG:(Int,JPanel), floors:List[(Floor, Int)]):Actor[ElevatorMessage] = {
    new GraphicElevator(pos, elevG, floors)
  }
  
  def repair(elev:ActorRef[ElevatorMessage]) {
    elev ! ERepair
  }
  
  def open(elev:ActorRef[ElevatorMessage]) {
    elev ! EOpen
  }

  def close(elev:ActorRef[ElevatorMessage]) {
    elev ! EClose
  }
  
  def stop(elev:ActorRef[ElevatorMessage]) {
    elev ! StopMessage
  }
  
  def move(elev:ActorRef[ElevatorMessage], dir:Direction) {
    elev ! EMove(dir)
  }
  
  def reset(elev:ActorRef[ElevatorMessage], state:ElevatorState, floor:Floor) {
    elev ! Reset(state, floor)
  }
  
  def set_controller(elev:ActorRef[ElevatorMessage], ePid:ActorRef[ElevatorMessage]) {
    elev ! EPid(ePid)
  }
  
  
  def approaching(elev:ActorRef[ElevatorMessage], floor:Floor) {
    elev ! EApproaching(floor)
  }

  def atFloor(elev:ActorRef[ElevatorMessage], floor:Floor) {
    elev ! EAt(floor)
  }

  def get_state(elev:ActorRef[ElevatorMessage]):(Int, ElevatorState, Floor) = {
 //   val res = ask(elev, getState)(1 second).mapTo[(Int, ElevatorState, Floor)]
    implicit val timeout = akka.util.Timeout(1 second)
    val res = elev ? getState
    Await.result(res, 1 second)
  }
  
  //TODO: code_change(_OldVsn, State, Data, _Extra)
  //TODO: terminate(_Reason, _State, {ENo, _Floor})
  
}