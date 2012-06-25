// changes made to this file

// Copyright 2012 Vibul Imtarnasan, David Bolton and Socko contributors.
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
package org.mashupbots.socko.examples.spdy

import org.mashupbots.socko.events.{SockoEvent, HttpRequestEvent}
import takka.actor.Actor
import java.util.Date

/**
 * Returns dynamic content
 */
class DynamicHandler extends Actor[SockoEvent] {
  def typedReceive = {
    case event: HttpRequestEvent =>
      val content = "<html>\n<body>\nDate and time is " + new Date().toString + "\n</body>\n</html>\n"
      event.response.write(content, "text/html; charset=UTF-8")
      context.stop(self)
  }
}

