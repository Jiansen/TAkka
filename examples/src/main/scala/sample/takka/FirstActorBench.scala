package sample.takka

import takka.actor._
import scala.reflect.runtime.universe

class BenchTimer{
  private var startTime:Long = _
  private var finishTime:Long = _
  
  def start = {
    startTime = System.currentTimeMillis()
  }
  def finish = {
    finishTime = System.currentTimeMillis()
  }
  def report = {
    println("elapse: "+(finishTime - startTime)+" milliseconds.");
  }
}

object FirstActorBench extends App {
  val typedsystem = ActorSystem("TypedSystem")
  
  val timer = new BenchTimer
  
  timer.start
  var i = 0
  while (i<10000){
//    typedsystem.actorOf(Props[String, sample.takka.MyActor])
    i = i+1
  }
  timer.finish
  timer.report
}