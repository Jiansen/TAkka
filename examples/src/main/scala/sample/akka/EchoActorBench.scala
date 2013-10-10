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

object EchoActorBench extends App {
  val untypedsystem = ActorSystem("UntypedSystem")
  val numberOfActors:Int = 90000
  val timer = new BenchTimer
  
  timer.start
  var i = 0
  while (i<numberOfActors){
    untypedsystem.actorOf(Props[sample.akka.EchoActor])
    i = i+1
  }
  timer.finish
  timer.report
  sys.exit()
}