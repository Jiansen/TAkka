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
package com.excilys.ebi.gatling.http

import com.excilys.ebi.gatling.http.ahc.GatlingAsyncHandler

import takka.actor.ActorRef

package object ahc {

	type HandlerFactory = (String, ActorRef[HttpEvent]) => GatlingAsyncHandler
}