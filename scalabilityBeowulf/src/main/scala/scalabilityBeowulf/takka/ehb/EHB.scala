/*
This is an implementation of hackbench in Erlang, a benchmark 
and stress test for Linux schedulers. The number of groups and 
the number of messages that each sender should send to each 
receiver in the same group are the two parameters that this 
benchmark receives.
 */
package scalabilityBeowulf.takka.ehb

import takka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.{BenchTimer, BenchCounter}
import akka.remote._
import com.typesafe.config.ConfigFactory
import scalabilityBeowulf.BeowulfConfig._

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
  val GSIZE = 20
}


case class MasterGO(groups:Int, loops:Int) extends MasterMsg
case class GroupGO(master:ActorRef[MasterMsg]) extends GroupMsg
case class GroupReady(g:ActorRef[GroupMsg]) extends MasterMsg
case class GroupDone(g:ActorRef[GroupMsg]) extends MasterMsg

class MasterActor extends Actor[MasterMsg]{
  val master = typedSelf
  val timer = new BenchTimer
  var gs:List[ActorRef[GroupMsg]] = _
  val readyCounter = new BenchCounter
  val doneCounter = new BenchCounter
  def typedReceive = {
    case MasterGO(groups, loops) =>
      readyCounter.set(groups)
      doneCounter.set(groups)
//      timer.start
      gs = (for (gid <- 1 to groups) yield {
        typedContext.actorOf(Props[GroupMsg](new GroupActor(master, loops)), EHBNodeConfig.ProcessNamePrefix+gid)
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

class GroupActor(master:ActorRef[MasterMsg], loops:Int) extends Actor[GroupMsg] {
  val gMaster = typedSelf
  val receiverDoneCounter = new BenchCounter
  receiverDoneCounter.set(EHBConstant.GSIZE)
  val rs:List[ActorRef[ReceiverMsg]] = (for (rid <- 1 to EHBConstant.GSIZE) yield {
    typedContext.actorOf(Props[ReceiverMsg].withCreator(new Receiver(gMaster, EHBConstant.GSIZE)), "receiver_"+rid)
  }).toList
  val ss:List[ActorRef[SenderMsg]] = (for (sid <- 1 to EHBConstant.GSIZE) yield {
    typedContext.actorOf(Props[SenderMsg].withCreator(new Sender(rs, loops)), "sender_"+sid)
  }).toList
  
  master ! GroupReady(typedSelf)
  
  def typedReceive = {
    case GroupGO(master) =>
      for (s<-ss) {
        s ! SenderGo(gMaster)
      }
    case ReceiverDone(r) =>
      receiverDoneCounter.decrement
      if(receiverDoneCounter.isZero){
        master ! GroupDone(typedSelf)
      }
  }
}

case class SenderGo(master:ActorRef[GroupMsg]) extends SenderMsg

class Sender(rs:List[ActorRef[ReceiverMsg]], loops:Int) extends Actor[SenderMsg] {
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

class SenderAck2 extends Actor[IamKeeypingUp]{
    def typedReceive = {
      case IamKeeypingUp(receiver) =>
        receiver ! EHBConstant.DATA
    }
}

case class Receiver(val gMaster:ActorRef[GroupMsg], var senderLeft:Int) extends Actor[ReceiverMsg]{
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
  private val groups = 3
  
  private val system = ActorSystem("EHBSystem", masterNodeConfig(EHBNodeConfig.WorkerNodePrefix, EHBNodeConfig.ProcessPathPrefix, EHBNodeConfig.ProcessNamePrefix, groups, nodes))  
  val master = system.actorOf(Props[MasterMsg, MasterActor], "EHBBenchActor")
  master ! MasterGO(groups,3)
}

object EHBNode extends App {
  private val nodeID:Int = args(0).toInt

  private val system = ActorSystem(EHBNodeConfig.WorkerNodePrefix+nodeID, WorkerNodeConfig(nodeID))
}

object EHBNodeConfig {
  val WorkerNodePrefix = "EHBNode"
  val ProcessPathPrefix = "EHBBenchActor"
  val ProcessNamePrefix = "EHBProcess"
}