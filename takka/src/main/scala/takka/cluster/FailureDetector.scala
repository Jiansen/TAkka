/*
   Copyright 2012 Jiansen HE

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
/**
 * Copied and modified from akka.cluster by Typesafe Inc. <http://www.typesafe.com>
 */
package takka.cluster

import akka.actor.Address

/**
 * Interface for Akka failure detectors.
 */
trait FailureDetector {

  /**
   * Returns true if the connection is considered to be up and healthy and returns false otherwise.
   */
  def isAvailable(connection: Address): Boolean

  /**
   * Returns true if the failure detector has received any heartbeats and started monitoring
   * of the resource.
   */
  def isMonitoring(connection: Address): Boolean

  /**
   * Records a heartbeat for a connection.
   */
  def heartbeat(connection: Address): Unit

  /**
   * Removes the heartbeat management for a connection.
   */
  def remove(connection: Address): Unit

  /**
   * Removes all connections and starts over.
   */
  def reset(): Unit
}
