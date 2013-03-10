package org.enmas.pomdp

/** Represents a Dec-POMDP or POSG.
  *
  * The set of agents allowed to connect to a server simulating this model
  * is defined by the agentConstraints member.  See the docs for AgentConstraint
  * for more information.
  *
  * An "Agent Type" shares:
  * 1) a set of available actions
  * 2) the reward conferred at each iteration
  *
  * The observation function, as visible from the signature, is allowed to
  * take into account the agent number for individual observations.
  * 
  * The transition function may return either a State object, or a
  * List of (State, Int) tuples, which is interpreted as a normalized
  * probability distribution.  The probability of each State component is the
  * corresponding Int component divided by the sum of all Int components.
  * States with non-positive Int components are ignored as impossible
  * transitions.  In the event that the transition function returns the empty
  * list, the next state is equal to the current.
  */
case class POMDP (
  name: String,
  description: String,
  agentConstraints: List[AgentConstraint],
  initialState: State,
  actionsFunction: (AgentType) => Set[Action],
  transitionFunction: (State, JointAction) => Either[State, List[(State, Int)]],
  rewardFunction: (State, JointAction, State) => AgentType => Float,
  observationFunction: (State, JointAction, State) => (Int, AgentType) => Observation
) {

  /** Checks that all supplied agent types are allowed and that
    * cardinalities do not exceed limits.
    *
    * This function is called by the server to determine whether
    * a new agent should be allowed to connect.
    */
  final def accomodatesAgents(agents: List[AgentType]): Boolean = {
    val allAllowed = agents.foldLeft(true){ (a,b)  => {
      a && { agentConstraints map(_.agentType) contains(b) }}}

    val allUnderLimit = agentConstraints.foldLeft(true){ (a,b)  => {
      a && { agents.filter(_ == b.agentType).length <= b.max }}}

    allAllowed && allUnderLimit
  }

  /** Checks that this model accomodates the agent set and that 
    * the agent set satisfies the minimum cardinalities.
    *
    * This is a more stringent test than the accomodatesAgents method.
    *
    * This function is called by the server to determine whether
    * the current agent set is sufficient to iterate the simulation.
    */
  final def isSatisfiedByAgents(agents: List[AgentType]) : Boolean = {
    accomodatesAgents(agents) && { agentConstraints.foldLeft(true){ (a,b)  => {
      a && agents.filter(_ == b.agentType).length >= b.min }}}
  }

  final override def toString() = name
}
