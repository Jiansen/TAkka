/**
 * Copyright 2011-2012 eBusiness Information, Groupe Excilys (www.excilys.com)
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
package com.excilys.ebi.gatling.core.action.builder

import com.excilys.ebi.gatling.core.action.{ system, StartAction }
import com.excilys.ebi.gatling.core.config.ProtocolConfigurationRegistry

import takka.actor.{ Props, ActorRef }
import com.excilys.ebi.gatling.core.session.Session

object StartActionBuilder {

	/**
	 * Creates a new StartActionBuilder
	 *
	 * @return A StartActionBuilder ready to use
	 */
	def startActionBuilder = new StartActionBuilder(null)
}

/**
 * Builder for StartAction
 *
 * @constructor create a StartActionBuilder with its next action
 * @param next the action to be executed after this one
 */
class StartActionBuilder(next: ActorRef[Session]) extends ActionBuilder {
	def withNext(next: ActorRef[Session]) = new StartActionBuilder(next)

	def build(protocolConfigurationRegistry: ProtocolConfigurationRegistry) = system.actorOf(Props(new StartAction(next)))
}