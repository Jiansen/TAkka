package sample.akka

import akka.actor._

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
  val untypedsystem = ActorSystem("UntypedSystem")
  
  val timer = new BenchTimer
  
  timer.start
  var i = 0
  while (i<10000){
    untypedsystem.actorOf(Props[sample.akka.MyActor])
    i = i+1
  }
  timer.finish
  timer.report
}