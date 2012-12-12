package scalability.takka.serialmsg

/*
Short description: Message proxying through a dispatcher

Arguments: P ( the number of receivers ), N ( the  number of messages ), 
L ( the data length )

Function: push

Long description: Spawns P receivers, 1 dispatcher and and P generators. The 
dispatcher forwards the messages that it receives from generators to the 
appropriate receiver. Each receiver receives messages from a specific generator 
and ignores them. Each generator sends N messages to a specific receiver. Each 
message contains a list of integers between 1 and L.
 */
import takka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.BenchTimer

sealed trait DispatcherMsg
sealed trait ReceiverMsg
sealed trait MasterMsg
sealed trait GeneratorMsg

case class Push(p:Int, n:Int, l:Int) extends MasterMsg
case class RecvDone(recv:ActorRef[ReceiverMsg]) extends MasterMsg
case class GeneratorDone(generator:ActorRef[GeneratorMsg], recv:ActorRef[ReceiverMsg]) extends DispatcherMsg 
case class GeneratorData(generator:ActorRef[GeneratorMsg], recv:ActorRef[ReceiverMsg], data:List[Int]) extends DispatcherMsg 

case class Do(master:ActorRef[MasterMsg]) extends GeneratorMsg
case class DoNewN(master:ActorRef[MasterMsg], newN:Int) extends GeneratorMsg

case class Done(generator:ActorRef[GeneratorMsg]) extends ReceiverMsg
case class Data(generator:ActorRef[GeneratorMsg], data:List[Int]) extends ReceiverMsg

class Receiver(master:ActorRef[MasterMsg]) extends Actor[ReceiverMsg] {
  def typedReceive = {
    case Done(_) =>
      master ! RecvDone(typedSelf)
    case _ =>
      // discard received message i.e. Data(generator:ActorRef, data:List[Int])
  }
}

class Dispatcher extends Actor[DispatcherMsg] {
  def typedReceive = {
    case GeneratorDone(generator:ActorRef[GeneratorMsg], recv:ActorRef[ReceiverMsg]) =>
      recv ! Done(generator)
    case GeneratorData(generator, recv, data) =>
      recv ! Data(generator, data)
  }
}

class Generator(recv:ActorRef[ReceiverMsg], disp:ActorRef[DispatcherMsg], n:Int, l:Int) extends Actor[GeneratorMsg] {
  val data = (1 to l) toList
  
  def generator_push_loop(recv:ActorRef[ReceiverMsg], disp:ActorRef[DispatcherMsg], n:Int, data:List[Int]):Unit = {    
    if (n == 0) {
      disp ! GeneratorDone(typedSelf, recv)
    }else{
      disp ! GeneratorData(typedSelf, recv, data)
      generator_push_loop(recv, disp, n-1, data)      
    }
  }
  
  def typedReceive = {
    case Do(master) =>
      generator_push_loop(recv, disp, n, data)
    case DoNewN(master, newN) =>
      generator_push_loop(recv, disp, newN, data)
  }
}

class PushActor extends Actor[MasterMsg] {
  val timer = new BenchTimer
  var p:Int = _
  def typedReceive = {
    case Push(p, n, l) => {
      this.p = p
      
      val recvs:List[ActorRef[ReceiverMsg]] = (for (i<-1 to p) yield {
        typedContext.actorOf(Props[ReceiverMsg, Receiver].withCreator(new Receiver(typedSelf)))
      }).toList

      val disp = typedContext.actorOf(Props[DispatcherMsg, Dispatcher])
      
      val gens = (for (recv<-recvs) yield {
        typedContext.actorOf(Props[GeneratorMsg, Generator].withCreator(new Generator(recv, disp, n, l)))
      }).toList
      
      this.timer.start
      for (gen <- gens) {gen ! Do(typedSelf)}
    }
    case RecvDone(recv:ActorRef[ReceiverMsg]) =>
      this.p -= 1
      if (this.p == 0) {
        this.timer.finish
        this.timer.report
        sys.exit()
      }
  }
}

object SerialMsg extends App {
  private val system = ActorSystem("RANSystem")

  val testActor = system.actorOf(Props[MasterMsg, PushActor], "RANTestActor")
  testActor ! Push(100, 1000, 2000)
}