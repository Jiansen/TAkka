package sample.elevatorController.akka

import akka.actor._

object ElevatorTest extends App{
  //println("Hello ")
  //util.start(2,6,5)
  //util.start(2,6,1)
  //val superAct = util.start_sup(2,6,5)
  //Thread.sleep(2000)
  // println("Hello ")
  //util.stop_sup(superAct)
  //util.start_sup(3,3,1)  
/*
  util.start_sup(1,6,2)  
  Thread.sleep(1000)
  scheduler.f_button_pressed(Floor(2))
  scheduler.f_button_pressed(Floor(3))  
  scheduler.f_button_pressed(Floor(6))    
  */
  //util.start_sup(1,6,2) 
  //ElevatorPlainEQC.prop_stop_where_requested_single_command(6, 4).check
  //ElevatorPlainEQC.prop_stop_where_requested_single(6, 4)
  
  ElevatorPlainEQC.prop_stop_where_requested.check

  /*
  val system1 = ActorSystem("Hello")
  val system2 = ActorSystem("Hello")  

  println(system1)
  println(system2)
  println(system1 == system2)
  */
}