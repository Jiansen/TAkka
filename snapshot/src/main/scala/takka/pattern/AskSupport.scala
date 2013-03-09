
package takka.pattern


import takka.actor._
import scala.concurrent.Future 
import akka.util.Timeout

trait AskSupport {

  implicit def ask[M](actorRef: ActorRef[M]): AskableActorRef[M] = new AskableActorRef(actorRef)

  def ask[M](actorRef: ActorRef[M], message: M)(implicit timeout: Timeout): Future[Any] = {
    akka.pattern.ask(actorRef.untypedRef, message)(timeout)
  }

  private[pattern] final class AskableActorRef[M](val actorRef: ActorRef[M]) {
    def ask(message: M)(implicit timeout: Timeout): Future[Any] = akka.pattern.ask(actorRef.untypedRef, message)(timeout)

    def ?(message: M)(implicit timeout: Timeout): Future[Any] = akka.pattern.ask(actorRef.untypedRef, message)(timeout)
  }
}
