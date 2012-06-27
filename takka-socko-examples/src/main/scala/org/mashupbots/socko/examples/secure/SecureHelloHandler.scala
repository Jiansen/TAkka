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
package org.mashupbots.socko.examples.secure

import java.util.Date

import org.mashupbots.socko.events.HttpRequestEvent

import takka.actor.Actor

import org.mashupbots.socko.events.SockoEvent

/**
 * Writes a greeting and terminates.
 */
// type error found
class SecureHelloHandler extends Actor[SockoEvent] {
  def typedReceive = {
    case event: HttpRequestEvent =>
      event.response.write("Hello from a Secure Socko (" + new Date().toString + ")")
      context.stop(self)
  }
}

