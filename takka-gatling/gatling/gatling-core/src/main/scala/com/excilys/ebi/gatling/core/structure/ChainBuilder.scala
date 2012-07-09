/**
 * Copyright 2012 Jiansen HE.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * 		http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * takka-galting is a deravative work of the galting project, developed by eBusiness Information, Groupe Excilys (www.excilys.com)
 */
package com.excilys.ebi.gatling.core.structure

import com.excilys.ebi.gatling.core.action.builder.ActionBuilder
import com.excilys.ebi.gatling.core.config.ProtocolConfigurationRegistry

import takka.actor.ActorRef
import com.excilys.ebi.gatling.core.session.Session
/**
 * ChainBuilder class companion
 */
object ChainBuilder {
	/**
	 * DSL helper that creates a new ChainBuilder
	 */
	def chain = new ChainBuilder(Nil, null)
}
/**
 * This class defines chain related methods
 *
 * @param actionBuilders the builders that represent the chain of actions of a scenario/chain
 * @param next the action that will be executed after this chain
 */
class ChainBuilder(actionBuilders: List[ActionBuilder], next: ActorRef[Session])
		extends AbstractStructureBuilder[ChainBuilder](actionBuilders) {

	private[core] def newInstance(actionBuilders: List[ActionBuilder]) = new ChainBuilder(actionBuilders, next)

	private[core] def getInstance = this

	/**
	 * Method that sets next action (used for chains)
	 *
	 * @param next the action to be executed after the chain
	 * @return the last built action
	 */
	private[core] def withNext(next: ActorRef[Session]) = new ChainBuilder(actionBuilders, next)

	/**
	 * Method that actually builds the scenario
	 *
	 * @param scenarioId the id of the current scenario
	 * @return the first action of the scenario to be executed
	 */
	private[core] def build(protocolConfigurationRegistry: ProtocolConfigurationRegistry) = buildChainedActions(next, protocolConfigurationRegistry)
}