package org.enmas.pomdp

/** Defines the allowed minimum and maximum cardinalities for an
  * AgentType in a POMDP model.
  * 
  * The constraint:
  * {{{
  * AgentConstraint('TypeOne, 2, 3)
  * }}}
  * specifies that a model requires 2 but not more than 3 agents
  * of type 'TypeOne in order to iterate.
  */
case class AgentConstraint(
  agentType: AgentType,
  min: Int,
  max: Int
)