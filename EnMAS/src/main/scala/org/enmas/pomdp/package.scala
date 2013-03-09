package org.enmas {

  /** Provides classes for the EnMAS POMDP framework.  EnMAS stands for
    * "Environment for Multi Agent Simulation".
    *
    * == About the Project ==
    * EnMAS is an Environment for Multi-Agent and
    * Team-Based Artificial Intelligence Research.  The project is guided 
    * by current research in MAS, particularly the DEC-POMDP model.
    *
    * The name is pronounced like "en masse".
    *
    * ===== The main goals of the project are: =====
    *
    * 1. Rapid prototyping of POMDP problems for research and teaching purposes
    *
    * 2. Orthogonality between theoretical models and implementation
    *
    * 3. Genericity: abstaining from assumptions about the problem domain
    *
    * == Acknowledgements ==
    *
    * This project was created and is maintained by Connor Doyle 
    * <[[mailto:connor.p.d@gmail.com connor.p.d@gmail.com]]> as part
    * of the Master of Software Engineering degree at the University of
    * Wisconsin - La Crosse under the advisement of 
    * Drs. [[http://cs.uwlax.edu/~mallen Marty Allen]] and
    * [[http://charity.cs.uwlax.edu Kenny Hunt]].
    *
    * The author would like to acknowledge the generosity of the Department
    * of Computer Science at the University of Wisconsin - La Crosse
    * and the National Science Foundation.
    *
    * == Usage Summary ==
    * The main class in this package is [[org.enmas.pomdp.POMDP]]. 
    *
    * ==== To create a new simulation: ====
    *
    * ==== To create a new agent: ====
    *
    * == Legal ==
    * <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/3.0/" target="_blank">
    *   <img alt="Creative Commons License" 
    *     style="border-width:0" 
    *     src="http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png" />
    * </a><br />
    * <span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">
    * EnMAS (Environment for Multi-Agent Simulation)</span>
    * by <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Connor Doyle</span>
    * is licensed under a 
    * <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/3.0/" target="_blank">
    * Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License</a>. */

  package object pomdp {
    type Action = Symbol
    type AgentType = Symbol
    type JointAction = List[AgentAction]
    type Observation = State

    /** This action is taken on an Agent's behalf when it is first
      * added to a simulation.
      */
    val NO_ACTION = Symbol("")

    /** Implicit conversion from State to Either[State, List[(State, Int)]]
      */
    implicit def state2Either(s: State): Either[State, List[(State, Int)]] = Left(s)

    /** Implicit conversion from List[(State, Int)] to Either[State, List[(State, Int)]]
      */
    implicit def stateList2Either(
      stateList: List[(State, Int)]
    ): Either[State, List[(State, Int)]] = Right(stateList)
    
  }

}
