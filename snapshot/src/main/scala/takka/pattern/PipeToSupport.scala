package takka.pattern

import scala.concurrent.{ Future, ExecutionContext }
import scala.util.{ Failure, Success }
import takka.actor.{ Status, ActorRef, Actor }
import language.implicitConversions

trait PipeToSupport {

  final class PipeableFuture[T](val future: Future[T])(implicit executionContext: ExecutionContext) {
    def pipeTo(recipient: ActorRef[T])(implicit sender: ActorRef[_] = Actor.noSender): Future[T] = {
      future onComplete {
        case Success(r) ⇒ recipient.untypedRef ! r
        case Failure(f) ⇒ recipient.untypedRef ! Status.Failure(f)
      }
      future
    }
    def to(recipient: ActorRef[T]): PipeableFuture[T] = to(recipient, Actor.noSender)
    def to(recipient: ActorRef[T], sender: ActorRef[_]): PipeableFuture[T] = {
      pipeTo(recipient)(sender)
      this
    }
  }

  /**
   * Import this implicit conversion to gain the `pipeTo` method on [[scala.concurrent.Future]]:
   *
   * {{{
   * import akka.pattern.pipe
   *
   * Future { doExpensiveCalc() } pipeTo nextActor
   *
   * or
   *
   * pipe(someFuture) to nextActor
   *
   * }}}
   */
  implicit def pipe[T](future: Future[T])(implicit executionContext: ExecutionContext): PipeableFuture[T] = new PipeableFuture(future)
}