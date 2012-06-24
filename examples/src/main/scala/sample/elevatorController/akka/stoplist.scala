package sample.elevatorController.akka

object stoplist {

/*  TODO:  does lastFloor matter?
%% add(Floor, LastFloor, StopList)
%%  Adds a stop at Floor to StopList, given that the last floor passed was
%%  LastFloor.
*/
  def add(floor:Floor, lastFloor:Floor, stops:List[Floor]): List[Floor] = {
    stops match {
      case Nil => List(floor)
      case f::rest => if (f==floor) stops else f :: add(floor, lastFloor, rest)
    }
  }

/*  
%% time_to(ToFloor, FromFloor, StopList)
%%  Gives an estimate of how long it will take to get to ToFloor, given
%%  that we've passed FromFloor and have to stop at all floors in StopList.
%%  The actual value need not be a correct time, all we need is a
%%  number which can be used to select the best elevator to choose to go to
%%  a floor.
*/
  def time_to(toFloor:Floor, fromFloor:Floor, stopList:List[Floor]):Int = {
    stopList.size
  }
  
  
}