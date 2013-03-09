package org.enmas.server

import org.enmas.pomdp._, org.enmas.messaging.AgentSpec

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
)