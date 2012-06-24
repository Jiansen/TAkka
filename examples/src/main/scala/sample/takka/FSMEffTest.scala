package sample.takka

import takka.actor._
import takka.actor.FSM._

sealed trait FSMState
case object A1 extends FSMState
case object B1 extends FSMState
case object C1 extends FSMState
case object D1 extends FSMState
case object E1 extends FSMState
case object F1 extends FSMState
case object G1 extends FSMState
case object Stop1 extends FSMState

sealed trait FSMEvent
case object Continue extends FSMEvent
case object StopFSM extends FSMEvent


object FSMTimer{
  var start:Long = 0
  var end:Long = 0
  
  def showElapsedTime {
    println("Elapsed Time:"+ (end-start) +" ms")
  }
}

class FSMExample extends Actor[FSMEvent] with FSM[FSMState, Null, FSMEvent]{
  this.startWith(A1, null)
  
  when(A1) {
    case Event(Continue, _) => goto(B1)
    case Event(StopFSM,_) => goto(Stop1)
  }
  when(B1) {
    case Event(Continue, _) => goto(C1)
    case Event(StopFSM,_) => goto(Stop1)
  }
  when(C1) {
    case Event(Continue, _) => goto(D1)
    case Event(StopFSM,_) => goto(Stop1)
  }
  when(D1) {
    case Event(Continue, _) => goto(E1)
    case Event(StopFSM,_) => goto(Stop1)
  }
  when(E1) {
    case Event(Continue, _) => goto(F1)
    case Event(StopFSM,_) => goto(Stop1)
  }
  when(F1) {
    case Event(Continue, _) => goto(G1)
    case Event(StopFSM,_) => goto(Stop1)
  }
  when(G1) {
    case Event(Continue, _) => goto(A1)
    case Event(StopFSM,_) => goto(Stop1)
  }
  when(Stop1){
    case _ =>
      FSMTimer.end = System.currentTimeMillis()
      FSMTimer.showElapsedTime
      stay()     
  }
  
  initialize
} 

object FSMEffTest extends App{
  val fsm = ActorSystem("FSM").actorOf(Props[FSMEvent, FSMExample])
  var i = 1000000
  FSMTimer.start = System.currentTimeMillis
  while (i != 0){
    fsm ! Continue
    i-=1
  }
  fsm ! StopFSM
  fsm ! StopFSM
}


/*
 * svn version 116
 * 
 *   No. of transactions    Time(ms)	Records
 *   10^5                   701.8		648 716 706 749 690
 *   10^6					2593.4		2031 2178 1964 4886 1908
 */