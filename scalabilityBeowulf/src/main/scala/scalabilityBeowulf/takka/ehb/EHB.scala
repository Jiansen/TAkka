/*
This is an implementation of hackbench in Erlang, a benchmark 
and stress test for Linux schedulers. The number of groups and 
the number of messages that each sender should send to each 
receiver in the same group are the two parameters that this 
benchmark receives.
 */
package scalabilityBeowulf.takka.ehb

import takka.actor.{TypedActor, ActorRef, ActorSystem, Props}
import util.{BenchTimer, BenchCounter}
import akka.remote._
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

import takka.chaos._
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy._
import akka.actor.OneForOneStrategy


sealed trait GroupMsg
sealed trait MasterMsg
sealed trait ReceiverMsg
sealed trait SenderMsg

case object SenderDone extends ReceiverMsg
case class ReceiverDone(r:ActorRef[ReceiverMsg]) extends GroupMsg
case class RUKeepingUp(from:ActorRef[IamKeeypingUp]) extends ReceiverMsg
case class IamKeeypingUp(receiver:ActorRef[ReceiverMsg]) extends SenderMsg

case class DummyDATA(data:List[String]) extends ReceiverMsg

object EHBConstant {
  val ACK = 20
  val DATA = DummyDATA(List("a", "b", "c", "d", "e", "f", "g",
                  "h", "i", "j", "k", "l") )
  val GSIZE = 10
}


case class MasterGO(groups:Int, loops:Int) extends MasterMsg
case object GroupGO extends GroupMsg
case class GroupInit(master:ActorRef[MasterMsg], loops:Int) extends GroupMsg
case class GroupReady(g:ActorRef[GroupMsg]) extends MasterMsg
case class GroupDone(g:ActorRef[GroupMsg]) extends MasterMsg

class MasterActor extends TypedActor[MasterMsg]{
  
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 2, withinTimeRange = 1 minute) {
      case e  =>
//        println("Error: "+e)
        Resume    
  }
    
    
  val master = typedSelf
  val timer = new BenchTimer
  var gs:List[ActorRef[GroupMsg]] = _
  val readyCounter = new BenchCounter
  val doneCounter = new BenchCounter
  def typedReceive = {
    case MasterGO(groups, loops) =>
      readyCounter.set(groups)
      doneCounter.set(groups)
      gs = (for (gid <- 1 to groups) yield {
        typedContext.actorOf(Props[GroupMsg, GroupActor], ProcessNamePrefix+gid)
      }).toList
      
      for (g <- gs) {
        g ! GroupInit(master, loops)
      }
    case GroupReady(g) =>   
      readyCounter.decrement
      if(readyCounter.isZero){
        
        if(util.Configuration.EnableChaos){
        import takka.chaos.ChaosMode._
        val chaos = ChaosMonkey(gs)
        chaos.setMode(Kill)
//        chaos.enableDebug
        chaos.start(1 second)
        }
        
        timer.start
       for (g<-gs) {
         g ! GroupGO
       }
      }
    case GroupDone(g) =>
      doneCounter.decrement
      if(util.Configuration.TraceProgress){
        println("Remaining Groups: "+doneCounter.get)
      }
      if(doneCounter.isZero){
        timer.finish
        timer.report
        sys.exit()
      }
  }
}

class GroupActor extends TypedActor[GroupMsg] {
  val gMaster = typedSelf
  val receiverDoneCounter = new BenchCounter

  val rs:List[ActorRef[ReceiverMsg]] = (for (rid <- 1 to EHBConstant.GSIZE) yield {
    typedContext.actorOf(Props[ReceiverMsg].withCreator(new Receiver(gMaster, EHBConstant.GSIZE)), "receiver_"+rid)
  }).toList
  var ss:List[ActorRef[SenderMsg]] = _
  var master:ActorRef[MasterMsg] = _
  var loops:Int =  _
  def typedReceive = {
    case GroupInit(master, loops) => {
      this.master = master
      this.loops = loops
      
      receiverDoneCounter.set(EHBConstant.GSIZE)
      this.ss = (for (sid <- 1 to EHBConstant.GSIZE) yield {
        typedContext.actorOf(Props[SenderMsg].withCreator(new Sender(rs, loops)), "sender_"+sid)
      }).toList
      master ! GroupReady(typedSelf)      
      
    }
    case GroupGO =>
      for (s<-ss) {
        s ! SenderGo(gMaster)
      }
    case ReceiverDone(r) =>
      receiverDoneCounter.decrement
      
      if(util.Configuration.TraceProgress){
            println("Group "+typedSelf+" has "+receiverDoneCounter.get+" remaining receivers to be finished.")  
      }
      
      if(receiverDoneCounter.isZero){
        master ! GroupDone(typedSelf)
      }
  }
  
//  override def postRestart(reason: Throwable): Unit = {
//     typedSelf ! GroupInit(master, loops)
//  }
}

case class SenderGo(master:ActorRef[GroupMsg]) extends SenderMsg

class Sender(rs:List[ActorRef[ReceiverMsg]], loops:Int) extends TypedActor[SenderMsg] {
  def typedReceive = {
    case SenderGo(master) =>
      sender(rs, loops)
  }
  
  def sender(rs:List[ActorRef[ReceiverMsg]], loop:Int):Unit = {
    if (loop == 0){
      for (r<-rs){
        r ! SenderDone
      }
    }else if (loop > EHBConstant.ACK){
      sender_ack(rs, EHBConstant.ACK)
      sender(rs, loop - EHBConstant.ACK)
    }else{
      for (r<-rs){
        r ! EHBConstant.DATA
        sender(rs, loop-1)
      }
    }
  }

  def sender_ack(rs:List[ActorRef[ReceiverMsg]], n:Int):Unit = {
    if(n == 2){
      val ack = typedContext.actorOf(Props[IamKeeypingUp, SenderAck2])
      for (r<-rs){
        r ! EHBConstant.DATA
        r ! RUKeepingUp(ack)
      }      
    }else{
      for (r <- rs){
        r ! EHBConstant.DATA
      }
      sender_ack(rs, n-1)
    }
  }
}

class SenderAck2 extends TypedActor[IamKeeypingUp]{
    def typedReceive = {
      case IamKeeypingUp(receiver) =>
        receiver ! EHBConstant.DATA
    }
}

case class Receiver(val gMaster:ActorRef[GroupMsg], var senderLeft:Int) extends TypedActor[ReceiverMsg]{
  val senderCounter = new BenchCounter
  senderCounter.set(senderLeft)
  def typedReceive = {
    case SenderDone => 
      senderCounter.decrement
      if (senderCounter.isZero)
        gMaster ! ReceiverDone(typedSelf)        
    case RUKeepingUp(from) =>
      from ! IamKeeypingUp(typedSelf)
    case _ => // do nothing	
  }
}


object EHBBench extends App{
  private val nodes:Int = args(0).toInt
  private val groups = 128
  
  private val system = ActorSystem("EHBSystem", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, groups, nodes))  
  val master = system.actorOf(Props[MasterMsg, MasterActor], ProcessPathPrefix)
  master ! MasterGO(groups,3)
}