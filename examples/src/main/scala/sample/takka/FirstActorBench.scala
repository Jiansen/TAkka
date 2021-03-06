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
  val numberOfActors:Int =10000
  val timer = new BenchTimer
  
  timer.start
  var i = 0
  while (i<numberOfActors){
    typedsystem.actorOf(Props[String, sample.takka.ServerActor])
    i = i+1
  }
  timer.finish
  timer.report
  sys.exit()
}