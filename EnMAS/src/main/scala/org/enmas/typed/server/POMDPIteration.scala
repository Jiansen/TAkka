package org.enmas.typed.server

import org.enmas.pomdp._, org.enmas.typed.messaging.AgentSpec

/** Represents one iteration of a POMDP.
  *
  * Here, ordinality == n means that this POMDPIteration is the
  * nth iteration of the simulation.
  *
  * The State member is the state of the simulation at the beginning
  * of this iteration step.
  */
case class POMDPIteration(
  ordinality: Long,
  observations: Set[(AgentSpec, State)],
  rewards: Set[(AgentSpec, Float)],
  actions: JointAction,
  state: State
) extends org.enmas.typed.messaging.ClientManagerMessage with org.enmas.typed.messaging.ClientMessage

// TODO: ??