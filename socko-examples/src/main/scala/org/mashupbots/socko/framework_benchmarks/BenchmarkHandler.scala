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
package org.mashupbots.socko.framework_benchmarks

import org.mashupbots.socko.events.HttpRequestEvent
import akka.actor.Actor //
import java.util.Date

class BenchmarkHandler extends Actor {
  def receive = { 
    case event: HttpRequestEvent =>
      event.response.write( World.toJson)
      context.stop(self)
  }
  
  object World {
    /**
    * Convert a World to Json string
    */
    val toJson = "{\"message\":\"Hello World!\"}"
  }
}

