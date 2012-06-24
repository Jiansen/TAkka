package sample.elevatorController.takka

import takka.actor._
import akka.util.duration._
import akka.dispatch.{Future, Await}
import akka.actor.Actor.Receive
//import pattern.ask
import akka.util.Timeout
import akka.util.duration._
import takka.nameserver._


sealed trait Command
object Stop extends Command
object Continue extends Command

sealed class SchedulerMessage
case class SetController(ENo:Int, EPid:ActorRef[ElevatorMessage]) extends SchedulerMessage
case class FButton(floor:Floor) extends SchedulerMessage
case class EButtonPressed(eNo:Int, floor:Floor) extends SchedulerMessage
case class Passing(eNo:Int, floor:Floor) extends SchedulerMessage
case class Open(eNo:Int, floor:Floor) extends SchedulerMessage
case class Closed(eNo:Int, floor:Floor) extends SchedulerMessage
case class Approaching(eNo:Int, floor:Floor) extends SchedulerMessage with SynMessage[Command]

object scheduler {
  //-record(sched_elevator, {number, pid, state, floor, stoplist = []}).
  class SchedElevator {
    var number:Int = _
    var pid:ActorRef[ElevatorMessage] = _
    var state:ElevatorState = _
    var floor:Floor = _
    var stoplist:List[Floor] = _
    
    override def toString():String = {
      "\n"+super.toString()+
      ":\n number "+number+
      "\n pid "+pid+
      "\n state "+state+
      "\n floor "+floor+
      "\n stoplist "+stoplist
    }
  }

  class SchedulerClass extends Actor[SchedulerMessage] {
    override def preStart() = {
      scheduler.update_actor(typedSelf)
    }
  
    override def postStop() = {
      scheduler.stop(typedSelf)
    }
    
    def typedReceive = synchronized {
      case Approaching(eNo, newFloor) => //Request for information on whether to stop or not.
        val NewElevs = change_floor(eNo, newFloor, Elevs)
//      sender ! Stop  //BUGGY CODE
        //if (get_stoplist(eNo, Elevs).contains(newFloor)) {// Buggy Code
        if (get_stoplist(eNo, Elevs).head == newFloor) {// Right Code        
          sender ! Stop
        }else{
          sender ! Continue
        }
      
        Elevs = NewElevs
      
      case SetController(eNo, ePid) => //Update contorl process information
        elevator.get_state(ePid) match {
          case (eno, estate, efloor) => transform(eno, estate, efloor) match {
            case (_ENo, state, floor) =>
              val nse = new SchedElevator()
              nse.number = eNo
              nse.pid = ePid
              nse.state = state
              nse.floor = floor
              nse.stoplist = Nil
            
              Elevs.find(e => e.number == eNo) match {
                case None => Elevs = nse::Elevs
                case Some(e) => 
                  e.number = nse.number
                  e.pid = nse.pid
                  e.state = nse.state
                  e.floor = nse.floor
              }            
          }
        }
      
      case FButton(floor) => //Call button handling.
        //val newElevs = schedule_elevator(floor, Elevs)
        //(newElevs)
        //Elevs = newElevs
        schedule_elevator(floor, Elevs)
    
      case EButtonPressed(eNo, floor) => //Elevator button handling.
        Elevs.find(e => e.number == eNo) match {
          case Some(e) => 
            val newElev = add_stop(floor, e)
            e.number = newElev.number
            e.pid = newElev.pid
            e.state = newElev.state
            e.floor = newElev.floor
          case None =>
            throw new Error("Internal Error: Elevator "+ eNo + " not fond in scheduler list.")
        }

      case Closed(eNo, floor) => //Doors have closed, start moving?
        Elevs.find(e => e.number == eNo) match {
          case Some(e) => 
            if (e.state == DoorOpen && e.stoplist == Nil) {
              e.state = DoorClosed
            }else{
              elevator.move(e.pid, direction(floor, e.stoplist.head))
              e.state = Moving
            }
          case None =>
            throw new Error("Internal Error: Elevator "+ eNo + " not fond in scheduler list.")
        }

      case Passing(eNo:Int, newFloor:Floor) => //Update state
        Elevs.find(e => e.number == eNo) match {
          case Some(e) => e.floor = newFloor
          case None =>
            throw new Error("Internal Error: Elevator "+ eNo + " not fond in scheduler list.")
        }

      case Open(eNo:Int, newFloor:Floor) => 
        Elevs.find(e => e.number == eNo) match {
          case Some(e) =>
            val newStopList = e.stoplist match {
              case nf::rest => rest
              case Nil => Nil
              case other => other
            }
            e.floor = newFloor
            e.state = DoorOpen
            e.stoplist = newStopList
          case None =>
            throw new Error("Internal Error: Elevator "+ eNo + " not fond in scheduler list.")
        }
    }    
  }
  
  //register an actor as "scheduler"
  var system = ActorSystem("scheduler")
  var pri_scheduler:ActorRef[SchedulerMessage] = _
  
  //Interface
  def start():ActorRef[SchedulerMessage] = {
    assert(pri_scheduler == null, "Scheduler has been started.")
    //pri_scheduler = system.actorOf(Props(new SchedulerClass()), "scheduler")
    pri_scheduler = system.actorOf(Props(new SchedulerClass()))
    pri_scheduler
  }
  
  def start_link():Actor[SchedulerMessage] = synchronized {
    //Thread.sleep(100)
    if(pri_scheduler == null || pri_scheduler.isTerminated){ // scheduler has not been started
      val actor = new SchedulerClass()
      pri_scheduler = actor.typedSelf
      //println("Creating "+pri_scheduler)
      actor
    }else{//wait for another 5 seconds
      Thread.sleep(5000)
      start_link()
    }
  }
  
  def update_actor(act:ActorRef[SchedulerMessage]) = synchronized {
    pri_scheduler = act
  }
  
  def stop(sch:ActorRef[SchedulerMessage]) = synchronized {
    //val a = system.actorFor("/user/scheduler")
    assert(pri_scheduler != null && !pri_scheduler.isTerminated,  "Scheduler has been terminated.")
    //system.stop(pri_scheduler)
    system.system.stop(sch.untypedRef)//TODO: 
    system.shutdown()
    system = ActorSystem("scheduler")
    //println("stopping "+ sch)
    pri_scheduler = null
  }
  
  private def init() = synchronized {
    Elevs = NameServer.get(TSymbol[ActorRef[ElevatorMessage]]('elev_sup)) match {
      case None => Nil
      case Some(_pid) =>  Nil  //TODO:
        
    }
  }
/*
%%  Initializes the scheduler by retrieving the elevator states for all
%%  elevators (if running supervised).
%%----------------------------------------------------------------------
init([]) ->
    Elevs =
	case whereis(elev_sup) of
	    undefined ->
		[];
	    _Pid ->
		lists:map(fun ({ENo, EPid, _, _}) ->
				  {_ENo, State, Floor} =
				      transform(elevator:get_state(EPid)),
				  #sched_elevator{number = ENo,
						  pid = EPid,
						  state = State,
						  floor = Floor}
			  end,
			  supervisor:which_children(elev_sup))
	end,
    {ok, Elevs}.
 */
  var Elevs:List[SchedElevator] = Nil//TODO: STATE  
/*
%% set_controller(ENo, EPid)
%%  Informs the scheduler that the control porcess EPid has been 
%%  started for elevator ENo.
*/
  def set_controller(ENo:Int, EPid:ActorRef[ElevatorMessage]) = synchronized {
    //println("scheduler "+pri_scheduler)
    pri_scheduler match {
      case dead if dead.isTerminated => new Error("Actor scheduler not found")      
      case sch:ActorRef[SchedulerMessage] => sch ! SetController(ENo, EPid) 
    }
  } 
  
/*
%% f_button_pressed(Floor)
%%  Informs the scheduler that the call button on the floor Floor
%%  has been pressed.
*/
  def f_button_pressed(floor:Floor) = synchronized {
    pri_scheduler match {
      case dead if dead.isTerminated => 
        new Error("Actor scheduler not found")
      case sch:ActorRef[SchedulerMessage] => 
        sys_event.f_button_pressed(floor)//MARK
        sch ! FButton(floor)
    } 
  }
  
/*
%% e_button_pressed(ENo, Floor)
%%  Informs the scheduler that the elevator button for the floor Floor
%%  has been pressed in elevator ENo.
*/
  def e_button_pressed(eNo:Int, floor:Floor) = synchronized {
    pri_scheduler match {
      case dead if dead.isTerminated => new Error("Actor scheduler not found")
      case sch:ActorRef[SchedulerMessage] => 
        sys_event.e_button_pressed(eNo, floor)
        sch ! EButtonPressed(eNo, floor) 
    } 
  }

/*
%% passing(Floor)
%%  Informs the scheduler that elevator ENo is passing the floor Floor.
*/
  def passing(eNo:Int, floor:Floor) = synchronized {
    pri_scheduler match {
      case dead if dead.isTerminated => new Error("Actor scheduler not found")
      case sch:ActorRef[SchedulerMessage] =>
        sch ! Passing(eNo, floor) 
    } 
  }
  
/*
%% open(Floor)
%%  Informs the scheduler that elevator ENo has stopped at the floor Floor.
*/
  def open(eNo:Int, floor:Floor) = synchronized {
    pri_scheduler match {
      case dead if dead.isTerminated => new Error("Actor scheduler not found")
      case sch:ActorRef[SchedulerMessage] =>
        sch ! Open(eNo, floor) 
    } 
  }

/*
%%  closed(ENo, Floor)
%%  Informs the scheduler that elevator ENo has closed the doors on floor
%%  Floor.
*/
  def closed(eNo:Int, floor:Floor) = synchronized {
    pri_scheduler match {
      case dead if dead.isTerminated => new Error("Actor scheduler not found")
      case sch:ActorRef[SchedulerMessage] =>
        sch ! Closed(eNo, floor) 
    } 
  }

/*
%% approaching(ENo, Floor)
%%  Informs the scheduler that elevator ENo is approaching the floor Floor,
%%  expects a reply indicating whether or not to stop.
%%  Uses catch to handle the possibility of a timeout.
*/
  def approaching(eNo:Int, floor:Floor):Command = synchronized {
    pri_scheduler match {
      case dead if dead.isTerminated => Stop
      case sch:ActorRef[SchedulerMessage] =>
        //sch ! Approaching(eNo, floor) //TODO: catch gen_server:call(scheduler, {approaching, ENo, Floor}, 200).
        implicit val timeout = Timeout(5 seconds)
        //val res = ask(sch, Approaching(eNo, floor))(200 millis).mapTo[Command]
        val res = sch.?[Command](Approaching(eNo, floor))
        Await.result(res, 200 millis)
    } 
  }
    
  
  
  // Internal functions
  // TODO: logic bugs?
  private def transform(eNo:Int, state:ElevatorState, floor:Floor):(Int, ElevatorState, Floor) = state match {
    case DoorOpen => (eNo, DoorOpen, floor)
    case DoorClosed => (eNo, DoorClosed, floor)
    case Moving => (eNo, Moving, floor)
    case Stopping => 
      //(eNo, Stopping, floor)
      (eNo, Moving, floor) //TODO: bug?
    case Uninitialized => throw new Exception("Should not receive state: "+state)
  }

  private def direction(from:Floor, to:Floor):Direction = (from, to) match {
    case (Floor(f), Floor(t)) =>
      if (f <= t) Up
      else Down
  }

  private def get_stoplist(eNo:Int, Elevs:List[SchedElevator]):List[Floor] = {
    Elevs.find(e => e.number == eNo) match {
      case Some(elev) => elev.stoplist
      case None => throw new Error("Internal Error: Elevator "+eNo+" not found.")
    }
  } 

  // TODO: bug in erlang version?
  private def change_floor(eNo:Int, newFloor:Floor, Elevs:List[SchedElevator]):List[SchedElevator] = {
    val elev = Elevs.find(e => e.number == eNo) match {
      case Some(elev) => elev
      case None => throw new Error("Internal Error: Elevator "+eNo+" not found.")
    }
    val newElevs:List[SchedElevator] = 
      for (e <- Elevs) yield {
        if (e.number == eNo){
          val elv = new SchedElevator()
          elv.number = eNo
          elv.pid = e.pid
          elv.state = e.state
          elv.floor = newFloor
          elv.stoplist = e.stoplist
          elv
        }else {e}
      }
    newElevs
  }		    
  
  //Make sure the Elev stops at Floor
  private def add_stop(floor:Floor, elev:SchedElevator):SchedElevator = {
    if (elev.floor == floor && elev.state == DoorOpen) {//Elevator is already there and open, ignore request
      elev
    }else if (elev.floor == floor && elev.state == DoorClosed) {//Elevator is there, but closed, open it.
      elevator.open(elev.pid)
      elev.state = DoorOpen
    }else if (elev.state == DoorClosed && elev.stoplist == Nil) {//Elevator is idle, start it moving.
      elevator.move(elev.pid,
	  	  direction(elev.floor, floor))
      elev.state = Moving
      elev.stoplist = List(floor)
    }else{//Otherwise just add to the stoplist.
      // println("add stop: "+ elev.stoplist)
      if (!elev.stoplist.contains(floor)){ // TODO: bug in Erlang version.  Need contain test
        elev.stoplist = elev.stoplist :+ floor
      }
    }
    elev
  }

  // Schedule one of the elevators to go to Floor.
  private def schedule_elevator(Floor:Floor, Elevs:List[SchedElevator]) = synchronized{
    check_open_elevator(Floor, Elevs)
  }

  // Check if there is an open elevator there already. If so, nothing
  // needs to be done.  Else, check check closed elevator at that floor
  // TODO: let it open?
  private def check_open_elevator(floor:Floor, elevs:List[SchedElevator]) = {
    if (elevs.exists(e => e.floor == floor && e.state == DoorOpen) ) { }// do nothing
    else check_closed_elevator(floor, elevs)
  }

  //Check if there is a closed (idle) elevator there. If so, open it.
  private def check_closed_elevator(floor:Floor, elevs:List[SchedElevator]) = {
    elevs.find(e => e.floor == floor && e.state == DoorClosed) match {
      case Some(e) =>
        elevator.open(e.pid)
        for (el <- elevs) {
          if (el.number == e.number) {
            e.state = DoorOpen
          }
        }
      case None => check_in_stoplist(floor, elevs)
    }    
  }

  // Check if any elevator is scheduled to stop there already. If so, nothing is done
  private def check_in_stoplist(floor:Floor, elevs:List[SchedElevator]) = {
    elevs.find(e => e.stoplist.contains(floor)) match {
      case Some(e) => elevs
      case None => add_to_a_stoplist(floor, elevs)
    }
  }	

  // Find the best elevator and schedule it to go there.
  private def add_to_a_stoplist(floor:Floor, elevs:List[SchedElevator]) = {
    elevs.map(elev => (stoplist.time_to(floor, elev.floor, elev.stoplist), elev)).sortBy(pair => pair._1) match {
      case (_time, selected)::rest =>
        val NewState = selected.state match {
          case DoorClosed =>
            elevator.move(selected.pid, this.direction(selected.floor, floor))
            Moving
          case state => state
        }
        val sl = stoplist.add(floor, selected.floor, selected.stoplist)
        for (e<-elevs) {
          if (e.number == selected.number) {
            e.state = NewState
            e.stoplist = sl
          }
        }
      case v => throw new Error("Internal Error: value "+ v + " cannot be matched." )
    }
  }
}
