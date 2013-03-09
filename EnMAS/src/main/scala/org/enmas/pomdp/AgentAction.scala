package org.enmas.pomdp

/** Represents an action taken by an Agent.
  */
case class AgentAction(
  agentNumber: Int,
  agentType: AgentType,
  action: Action
)
