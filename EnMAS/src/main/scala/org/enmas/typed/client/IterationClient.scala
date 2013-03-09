package org.enmas.typed.client
import org.enmas.typed.server.POMDPIteration

abstract class IterationClient extends Client {
  def handleIteration(iteration: POMDPIteration)
//  final def receive = { case i: POMDPIteration  ⇒ handleIteration(i) }
  final def typedReceive = { case i: POMDPIteration  ⇒ handleIteration(i) }  
}
