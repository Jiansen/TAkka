package org.enmas.client
import org.enmas.server.POMDPIteration

abstract class IterationClient extends Client {
  def handleIteration(iteration: POMDPIteration)
  final def receive = { case i: POMDPIteration  => handleIteration(i) }
}
