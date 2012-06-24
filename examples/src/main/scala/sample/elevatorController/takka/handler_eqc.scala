package sample.elevatorController.takka

object handler_eqc extends EventHandler{
  private class State {
    var floors:Int = -1
    var elevs:List[Elevator] = Nil
    var events:List[EQCEvent] = Nil
    
    def setFloors(f:Int) { this.floors = f }
    def setElevs(es:List[Elevator]) { this.elevs = es }
    
    def addEvents(e:EQCEvent) = {
      this.events = this.events ++ List(e)
    }
    def clearEvents() = {this.events = Nil}
  }
  private class Elevator(var nr:Int, var floor:Floor) {
    override def equals (arg0: Any): Boolean = arg0 match {
      case e:Elevator => e.nr == this.nr && e.floor == this.floor
      case _ => false
    }
  }


  
  private var state:State = _
  
  /*
%% init([Floor, NFloors, EPids])
%%  Initializes the display, with the elevator initially at Floor and
%%  a total of NFloors floors. EPids is a list of the elevator control
%%  process pids.
  */
  override def init(arg:Any):EventHandler = arg match {
    case (iFloor:Int, nFloors:Int, nElevs:Int) => 
      state = new State      
      val elevs =
        for (elev <- 1 to nElevs) yield {
          new Elevator(elev, Floor(iFloor))
        }
      state.setFloors(nFloors)
      state.setElevs(elevs.toList)
      return this
  }


  override def handle_event(event:Event) = event match {
    
    case OpenEvent(eNo) => 
      val elev = get_elev(eNo, state.elevs)
      state.addEvents(OpenEQCEvent(elev.nr, elev.floor))
    case CloseEvent(eNo) => Unit
    case MoveEvent(eNo, dir) => Unit
    case StoppingEvent(eNo) => Unit
    case ControllerStartedEvent(eNo, ePid) => Unit
    
    case ApproachingEvent(eNo, floor) =>
      val elev = get_elev(eNo, state.elevs)
      state.elevs = state.elevs.filterNot(e => e.nr == eNo) ++ List(new Elevator(elev.nr, floor))
    case ResetEvent(eNo, state, floor) => Unit
    case StoppedEvent(eNo, floor) => Unit
    case PassingEvent(eNo, floor) => Unit
    
    case EButtonPressedEvent(eNo, floor) =>
      state.addEvents(ElevatorEQCEvent(eNo, floor))
//      if(state.events == Nil) {println("EButtonPressedEvent")}   
    case FButtonPressedEvent(floor) =>
      state.addEvents(FloorEQCEvent(floor))
//      if(state.events == Nil) {println("FButtonPressedEvent")}   
      
    case RepairEvent(eNo:Int) => Unit
  }
  
/*
%% handle_call not used
%%----------------------------------------------------------------------
handle_call(opened, State) ->
    {ok, State#state.events, State#state{events=[]}};
handle_call(Request, State) ->
    {ok, ok, State}.
*/  
  def opened():List[EQCEvent] = {//return events && reset states
    val result = state.events
    state.events = Nil
    return result
  }
  
  def getevents():List[EQCEvent] = {//return events
    state.events
  }
  
  private def get_elev(nr:Int,elevs:List[Elevator]):Elevator = elevs match {
    case elev::elevs =>
      if (nr == elev.nr) {elev}
      else {get_elev(nr, elevs)}
    case Nil => throw new Error("Internal Error: get_elev from an empty list.");
  }

  
}