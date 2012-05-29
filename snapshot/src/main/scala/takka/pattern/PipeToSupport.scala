package takka.pattern

import akka.dispatch.Future
//import akka.actor.{ Status, ActorRef }
import takka.actor.ActorRef
import akka.actor.Status

trait PipeToSupport {

  final class PipeableFuture[T](val future: Future[T]) {
    def pipeTo(recipient: ActorRef[T]): Future[T] =
      future onComplete {
        case Right(r) => recipient ! r
        case Left(f)  => recipient.untyped_ref ! Status.Failure(f) //TODO:
      }

    def to(recipient: ActorRef[T]): PipeableFuture[T] = {
      pipeTo(recipient)
      this
    }
  }

  /**
   * Import this implicit conversion to gain the `pipeTo` method on [[akka.dispatch.Future]]:
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
  implicit def pipe[T](future: Future[T]): PipeableFuture[T] = new PipeableFuture(future)
}