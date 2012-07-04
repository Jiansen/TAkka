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
package org.mashupbots.socko.routes

import java.nio.charset.Charset
import org.jboss.netty.channel.Channel
import org.mashupbots.socko.events.EndPoint
//import org.mashupbots.socko.events.SockoEvent
import org.mashupbots.socko.events.HttpRequestEvent
/**
 *  Context for testing routing
 */
object TestContext {
  // -- HttpRequest is a case class
  // -- case-to-case inheritance results to compiler warning
  // -- we use pattern extractor instead
  def unapply(request:HttpRequestEvent):Option[EndPoint] = {
    Some(request.endPoint)
  }
  
  def apply(endPoint:EndPoint):HttpRequestEvent = {
    import org.jboss.netty.handler.codec.http._
    val request = new DefaultHttpRequest(new HttpVersion("HTTP/1.1", true),
        new HttpMethod(endPoint.method),
        endPoint.uri)
    
    request.addHeader("HOST", endPoint.host)
    
    
    HttpRequestEvent(
        null,
        request,
        null)
  }
}

/*
case class TestContext(endPoint: EndPoint) extends SockoEvent {
  val channel: Channel = null
  def readStringContent(): String = ""
  def readStringContent(charset: Charset): String = ""
  def readBinaryContent(): Array[Byte] = null
}
*/
