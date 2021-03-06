package sample.barbershop.takka

//import akka.actor.{PoisonPill, ActorRef}
import akka.actor.PoisonPill
import takka.actor.ActorRef

case class Barbershop(sign: ActorRef[SignMessages], lounge: ActorRef[ChairsMessages], line: ActorRef[LineMessage], barbers: List[ActorRef[BarberMessages]], tracker: ActorRef[TrackerMessages]) {
  val parts = List(sign, lounge, line, tracker) ::: barbers

  
  def start: Unit = {
//    parts foreach (_.start) // already started
    parts foreach (_.untypedRef ! this) // ???  refine this
  }
  

  
//  def stop: Unit = parts foreach (_ ! PoisonPill)
  def stop: Unit = parts foreach (_.untypedRef ! PoisonPill)  // TODO: refine this

}