/*
This is an implementation of hackbench in Erlang, a benchmark 
and stress test for Linux schedulers. The number of groups and 
the number of messages that each sender should send to each 
receiver in the same group are the two parameters that this 
benchmark receives.
 */
package scalability.akka.ehb

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.{BenchTimer, BenchCounter}
import scala.concurrent.ops.spawn

case object SenderDone
case class ReceiverDone(r:ActorRef)
case class RUKeepingUp(from:ActorRef)
case class IamKeeypingUp(receiver:ActorRef)


object EHBConstant {
  val ACK = 20
  val DATA = List("a", "b", "c", "d", "e", "f", "g",
                  "h", "i", "j", "k", "l") 
  val GSIZE = 20
}

case class MasterGO(groups:Int, loops:Int)
case class GroupGO(master:ActorRef)
case class GroupReady(g:ActorRef)
case class GroupDone(g:ActorRef)

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
//      timer.start
      gs = (for (gid <- 1 to groups) yield {
        context.actorOf(Props().withCreator(new GroupActor(master, loops)),"group_"+gid)
      }).toList
    case GroupReady(g) =>   
      readyCounter.decrement
      if(readyCounter.isZero){
        timer.start
       for (g<-gs) {
         g ! GroupGO(master)
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

class GroupActor(master:ActorRef, loops:Int) extends Actor {
  val gMaster = self
  val receiverDoneCounter = new BenchCounter
  receiverDoneCounter.set(EHBConstant.GSIZE)
  val rs:List[ActorRef] = (for (rid <- 1 to EHBConstant.GSIZE) yield {
    context.actorOf(Props().withCreator(new Receiver(gMaster, EHBConstant.GSIZE)), "receiver_"+rid)
  }).toList
  val ss:List[ActorRef] = (for (sid <- 1 to EHBConstant.GSIZE) yield {
    context.actorOf(Props().withCreator(new Sender(rs, loops)), "sender_"+sid)
  }).toList
  
  master ! GroupReady(self)
  
  def receive = {
    case GroupGO(master) =>
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

case class SenderGo(master:ActorRef)

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

object EHB extends App{
  val system = ActorSystem("EHB")
  val master = system.actorOf(Props[MasterActor], "master")
  master ! MasterGO(3,3)
}