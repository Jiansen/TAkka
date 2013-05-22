/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */

package takka

package object routing {
  /**
   * Routing logic, partial function from (sender, message) to a
   * set of destinations.
   */
  type Route[M] = PartialFunction[(takka.actor.ActorRef[M], Any), Iterable[Destination[M]]]
}
