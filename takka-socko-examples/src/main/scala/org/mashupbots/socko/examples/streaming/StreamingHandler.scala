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
package org.mashupbots.socko.examples.streaming

import java.util.Date

import org.mashupbots.socko.events.HttpRequestEvent
import org.mashupbots.socko.infrastructure.CharsetUtil

import takka.actor.TypedActor //

/**
 * Streams a greeting and stops.
 */
class StreamingHandler extends TypedActor[HttpRequestEvent] {
  def typedReceive = {
    case event => // : HttpRequestEvent =>
      
      event.response.writeFirstChunk("text/plain; charset=UTF-8")
      
      event.response.writeChunk(("Hello from Socko (" + new Date().toString + ")\n").getBytes(CharsetUtil.UTF_8))
      event.response.writeChunk("This is the second line in the second chunk\n".getBytes(CharsetUtil.UTF_8))
      event.response.writeChunk("This is the third line ... ".getBytes(CharsetUtil.UTF_8))
      event.response.writeChunk("split into 2 chunks.".getBytes(CharsetUtil.UTF_8))
      
      event.response.writeLastChunk()
            
      typedContext.stop(typedSelf)
  }
}

