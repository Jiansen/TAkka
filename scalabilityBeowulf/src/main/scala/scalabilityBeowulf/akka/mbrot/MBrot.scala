package scalabilityBeowulf.akka.mbrot

/*
This benchmark extrapolates the coordinates of a 2-D complex 
plane that correspond to the pixels of a 2-D image of a specific 
resolution. For each one of these points, the benchmark determines 
whether the point belongs to the Mandelbrot set or not. The total 
set of points is divided among a number of workers. The benchmark 
is parameterized by the dimensions of the image.
 */
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.{BenchTimer, BenchCounter}
import akka.remote._
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

import akka.chaos._
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy._
import akka.actor.OneForOneStrategy

sealed trait SupMsg
sealed trait WorkerMsg
case class GO(n:Int, np:Int) extends SupMsg
case class Work(n:Int, master:ActorRef) extends WorkerMsg

object MBrotCons{
  val MAXITER = 255
  val LIM_SQR = 4.0
  val RL = 2.0
  val IL = 2.0
}

class WorkerSup extends Actor{
  val counter = new BenchCounter
  val timer = new BenchTimer
  
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
      case e  =>
        Resume    
  }
  
  def receive = {
      case GO(n, np) =>{
        counter.set(np)
        
        val workers = (for(i<-1 to np) yield {
          context.actorOf(Props[Worker], ProcessNamePrefix+i)
        }).toList
        
        if(util.Configuration.EnableChaos){
          import akka.chaos.ChaosMode._
          val chaos = ChaosMonkey(workers)
          chaos.setMode(Kill)
//          chaos.enableDebug
          chaos.start(1 second)
        }
        timer.start
        for (worker <- workers) {
          worker ! Work(n, self)
        }
      }
      case Done(_) =>
        counter.decrement
        if(counter.isZero){
          timer.finish
          timer.report
          sys.exit()
        }
  }
}

case class Done(worker:ActorRef) extends SupMsg

class Worker extends Actor {
  def receive = {
    case Work(n, master) =>
      rows(n, n)
      master ! Done(self)
    case _ => 
  }
  
  def rows(w:Int, h:Int):Unit = rows(w, h, h)
  def rows(w:Int, h:Int, hi:Int):Unit = {
    if(hi>0){
      cols(w, h, hi)
      rows(w, h, hi-1)
    }
  }

  def cols(w:Int, h:Int, hi:Int):Unit = {
    cols(w,h,w,hi)
  }
  def cols(w:Int, h:Int, wi:Int, hi:Int):Unit = {
    if(wi>0){
      // transform X and Y pixel to mandelbrot coordinates
      val x = (wi - 1)/w*(2*MBrotCons.RL) - MBrotCons.RL
      val y = (hi - 1)/h*(2*MBrotCons.IL) - MBrotCons.IL
      // do mandelbrot
      mbrot(x, y)
      cols(w, h, wi - 1, hi)
    }
  }

  def mbrot(x:Double, y:Double):Double = {mbrot(x, y, x, y, 0)}
  def mbrot(x0:Double, y0:Double, x1:Double, y1:Double, i:Double):Double = {
    if (i<MBrotCons.MAXITER && (x1*x1+y1*y1) <= MBrotCons.LIM_SQR){
      val x2 = x1*x1 - y1*y1 +x0
      val y2 = 2*x1*y1 +y0
      mbrot(x0, y0, x2, y2, i+1)
    }else{
      i
    }
  }  
}

object MBrotBench extends App{
  private val nodes:Int = args(0).toInt
  private val processes = 256
  
  private val system = ActorSystem("MBrotSystem", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, processes, nodes))  
  val master = system.actorOf(Props[WorkerSup], ProcessPathPrefix)
  master ! GO(1000, processes)
}
