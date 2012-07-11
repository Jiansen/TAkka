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
package com.excilys.ebi.gatling.core.action.builder

import com.excilys.ebi.gatling.core.action.{ system, SimpleAction }
import com.excilys.ebi.gatling.core.config.ProtocolConfigurationRegistry
import com.excilys.ebi.gatling.core.session.Session

import takka.actor.{ Props, ActorRef }

object SimpleActionBuilder {

	/**
	 * Creates a simple action builder
	 *
	 * @param sessionFunction the function that will be executed by the built simple action
	 */
	def simpleActionBuilder(sessionFunction: Session => Session) = new SimpleActionBuilder(sessionFunction, null)
}

/**
 * Builder for SimpleAction
 *
 * @constructor creates a SimpleActionBuilder
 * @param sessionFunction the function that will be executed by the simple action
 * @param next the action that will be executed after the simple action built by this builder
 */
class SimpleActionBuilder(sessionFunction: Session => Session, next: ActorRef[Session]) extends ActionBuilder {

	def withNext(next: ActorRef[Session]) = new SimpleActionBuilder(sessionFunction, next)

	def build(protocolConfigurationRegistry: ProtocolConfigurationRegistry) = system.actorOf(Props(new SimpleAction(sessionFunction, next)))
}