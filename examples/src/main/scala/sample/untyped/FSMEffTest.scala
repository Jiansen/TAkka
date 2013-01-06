package sample.untyped

import akka.actor._
import akka.actor.FSM._
import scala.concurrent.duration._

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

class ExampleFSM extends Actor with FSM[FSMState, Null]{
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
  val fsm = ActorSystem("FSM1").actorOf(Props[ExampleFSM])
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
 * svn version 118
 * 
 *   No. of transactions    Time(ms)	Records
 *   10^5                   744.2		594, 620, 611, 584, 598
 *   10^6					2076.8		2050, 1956, 2066, 2026, 2286
 */
