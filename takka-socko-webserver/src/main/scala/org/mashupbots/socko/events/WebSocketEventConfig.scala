//
// Copyright 2012 Jiansen HE.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

// takka-socko is a derivative work of the socko project that is developed by
//   Vibul Imtarnasan, David Bolton and Socko contributors.
package org.mashupbots.socko.events

import takka.actor.ActorRef //import akka.actor.ActorRef
import org.mashupbots.socko.infrastructure.WebLogEvent
/**
 * Web Socket configuration used by [[org.mashupbots.socko.events.WebSocketFrameEvent]]
 *
 * @param serverName Name of this instance of the Socko Web Server
 * @param webLogWriter Actor to which web log events to be sent
 */
case class WebSocketEventConfig(
  serverName: String,
  webLogWriter: Option[ActorRef[WebLogEvent]]) {
}