/*
This is an implementation of hackbench in Erlang, a benchmark 
and stress test for Linux schedulers. The number of groups and 
the number of messages that each sender should send to each 
receiver in the same group are the two parameters that this 
benchmark receives.
 */
package scalabilityBeowulf.akka.ehb

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.{BenchTimer, BenchCounter}
import akka.remote._
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

sealed trait GroupMsg
sealed trait MasterMsg
sealed trait ReceiverMsg
sealed trait SenderMsg

case object SenderDone extends ReceiverMsg
case class ReceiverDone(r:ActorRef) extends GroupMsg
case class RUKeepingUp(from:ActorRef) extends ReceiverMsg
case class IamKeeypingUp(receiver:ActorRef) extends SenderMsg

case class DummyDATA(data:List[String]) extends ReceiverMsg

object EHBConstant {
  val ACK = 20
  val DATA = DummyDATA(List("a", "b", "c", "d", "e", "f", "g",
                  "h", "i", "j", "k", "l") )
  val GSIZE = 10
}


case class MasterGO(groups:Int, loops:Int) extends MasterMsg
case object GroupGO extends GroupMsg
case class GroupInit(master:ActorRef, loops:Int) extends GroupMsg
case class GroupReady(g:ActorRef) extends MasterMsg
case class GroupDone(g:ActorRef) extends MasterMsg

class MasterActor extends Actor{
  val master = self
  val timer = new BenchTimer
  var gs:List[ActorRef] = _
  val readyCounter = new BenchCounter
  val doneCounter = new BenchCounter
  def receive = {
    case MasterGO(groups, loops) =>
      readyCounter.set(groups)
      doneCounter.set(groups)
      gs = (for (gid <- 1 to groups) yield {
        context.actorOf(Props[GroupActor], ProcessNamePrefix+gid)
      }).toList
      for (g <- gs) {
        g ! GroupInit(master, loops)
      }
    case GroupReady(g) =>   
      readyCounter.decrement
      if(readyCounter.isZero){
        timer.start
       for (g<-gs) {
         g ! GroupGO
       }
      }
    case GroupDone(g) =>
      doneCounter.decrement
//      println("FINISH " + doneCounter.isZero) 
      if(doneCounter.isZero){
        timer.finish
        timer.report
        sys.exit()
      }
  }
}

class GroupActor extends Actor {
  val gMaster = self
  val receiverDoneCounter = new BenchCounter
  receiverDoneCounter.set(EHBConstant.GSIZE)
  val rs:List[ActorRef] = (for (rid <- 1 to EHBConstant.GSIZE) yield {
    context.actorOf(Props().withCreator(new Receiver(gMaster, EHBConstant.GSIZE)), "receiver_"+rid)
  }).toList
  var ss:List[ActorRef] = _
  var master:ActorRef = _
  def receive = {
    case GroupInit(master, loops) => {
      this.ss = (for (sid <- 1 to EHBConstant.GSIZE) yield {
        context.actorOf(Props().withCreator(new Sender(rs, loops)), "sender_"+sid)
      }).toList
      this.master = master
      master ! GroupReady(self)      
    }
    case GroupGO =>
      for (s<-ss) {
        s ! SenderGo(gMaster)
      }
    case ReceiverDone(r) =>
      receiverDoneCounter.decrement
      if(receiverDoneCounter.isZero){
        master ! GroupDone(self)
      }
  }
}

case class SenderGo(master:ActorRef) extends SenderMsg

class Sender(rs:List[ActorRef], loops:Int) extends Actor {
  def receive = {
    case SenderGo(master) =>
      sender(rs, loops)
  }
  
  def sender(rs:List[ActorRef], loop:Int):Unit = {
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

  def sender_ack(rs:List[ActorRef], n:Int):Unit = {
    if(n == 2){
      val ack = context.actorOf(Props[SenderAck2])
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

class SenderAck2 extends Actor{
    def receive = {
      case IamKeeypingUp(receiver) =>
        receiver ! EHBConstant.DATA
    }
}

case class Receiver(val gMaster:ActorRef, var senderLeft:Int) extends Actor{
  val senderCounter = new BenchCounter
  senderCounter.set(senderLeft)
  def receive = {
    case SenderDone => 
      senderCounter.decrement
      if (senderCounter.isZero)
        gMaster ! ReceiverDone(self)        
    case RUKeepingUp(from) =>
      from ! IamKeeypingUp(self)
    case _ => // do nothing
  }
}


object EHBBench extends App{
  private val nodes:Int = args(0).toInt
  private val groups = 32
  
  private val system = ActorSystem("EHBSystem", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, groups, nodes))  
  val master = system.actorOf(Props[MasterActor], ProcessPathPrefix)
  master ! MasterGO(groups,3)
}