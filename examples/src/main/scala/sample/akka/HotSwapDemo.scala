package sample.akka
import akka.actor._
import akka.event.Logging

case object Swap

class Swapper extends Actor{
  import context._
  val log = Logging(system, this)

  def receive = {
    case Swap =>
      log.info("Hi")
      become({
        case Swap =>
          log.info("Ho")
          unbecome()}, discardOld=false)
  }
}

object SwapperApp extends App {
  val system = ActorSystem("SwapperSystem")
  val swap = system.actorOf(Props[Swapper], "swapper")
  swap ! Swap
  swap ! Swap
  swap ! Swap
  swap ! Swap
}

/*  Terminal output:
  Hi
  Ho
  Hi
  Ho
 */