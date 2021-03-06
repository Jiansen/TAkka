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

import org.mashupbots.socko.routes._
import org.mashupbots.socko.infrastructure.Logger
import org.mashupbots.socko.webserver.WebServer
import org.mashupbots.socko.webserver.WebServerConfig

// import akka.actor.actorRef2Scala
import akka.actor.ActorSystem //
import akka.actor.Props //
import org.mashupbots.socko.events.HttpRequestEvent
/**
 * This example shows how to setup a simple route and create a simple processor actor.
 *  - Run this class as a Scala Application
 *  - Open your browser and navigate to `http://localhost:8888/`
 *
 * Socko uses Netty to handle incoming requests and Akka to process them
 *  - Incoming requests are converted into Socko events using threads from the Netty thread pool
 *  - Your `routes` are then called to dispatch the event for processing
 *  - Inside our route definition, we instance a new `HelloHandler` actor and pass the event to it
 *  - The `HelloHandler` actor is executed in Akka default thread pool. This frees up the Netty thread pool to 
 *    undertake more networking activities.
 */
object BenchmarkApp extends Logger {

  val actorSystem = ActorSystem("BenchmarkAppActorSystem")

  val routes = Routes({
    case GET(request) => request match {

      case (GET(Path("/json")) ) => {
        actorSystem.actorOf(Props[BenchmarkHandler]) ! request
      }
    }
  })

  def main(args: Array[String]) {
    val webServer = new WebServer(WebServerConfig(), routes, actorSystem)
    webServer.start()

    Runtime.getRuntime.addShutdownHook(new Thread {
      override def run { webServer.stop() }
    })

//    System.out.println("Open your browser and navigate to http://localhost:8888")
  }
}