package sample.elevatorController.takka

import akka.actor._

object ElevatorTest extends App{
  
  /*
   * Start and Stop
   */
  //println("Hello ")
  //util.start(2,6,5)
  //util.start(2,6,1)
  //val superAct = util.start_sup(2,6,5)
  //Thread.sleep(2000)
  // println("Hello ")
  //util.stop_sup(superAct)
  //util.start_sup(3,3,1)  
  /*
   *   Buttons
   */
  /*
  util.start_sup(1,6,2)  
  Thread.sleep(1000)
  scheduler.f_button_pressed(Floor(2))
  scheduler.f_button_pressed(Floor(3))  
  scheduler.f_button_pressed(Floor(6))    
  */
  
  /*
   *  Quick Check
   */
  //util.start_sup(1,6,2) 
  //ElevatorPlainEQC.prop_stop_where_requested_single_command(6, 4).check
  //ElevatorPlainEQC.prop_stop_where_requested_single(6, 4)
  
  // ElevatorPlainEQC.prop_stop_where_requested.check
  
  /*
   * OTP check: let actor fail.
   */
  val superAct = util.start_sup(2,6,3)

  
}

/*
 * svn version:
 *   when start an elevator
 *     Print out
 *       Starting ActorRef[ElevatorMessage]: akka://util/user/sim_sup/system_sup/elev_sup/Elevator_N
 *   
 *   when restart an elevator 
 *     Print out 
 *       Restarting ActorRef[ElevatorMessage]: akka://util/user/sim_sup/system_sup/elev_sup/Elevator_N
         Starting ActorRef[ElevatorMessage]: akka://util/user/sim_sup/system_sup/elev_sup/Elevator_N
 */ 
