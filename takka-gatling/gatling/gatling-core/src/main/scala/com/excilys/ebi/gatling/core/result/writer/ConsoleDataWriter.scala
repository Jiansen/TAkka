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
package com.excilys.ebi.gatling.core.result.writer

import java.lang.System.currentTimeMillis

import com.excilys.ebi.gatling.core.action.EndAction.END_OF_SCENARIO
import com.excilys.ebi.gatling.core.action.StartAction.START_OF_SCENARIO
import com.excilys.ebi.gatling.core.result.message.{ RequestRecord, InitializeDataWriter, FlushDataWriter }
import com.excilys.ebi.gatling.core.result.message.RequestStatus.{ OK, KO }

import grizzled.slf4j.Logging

import takka.actor.FSM
import com.excilys.ebi.gatling.core.result.message.DataWriterMessage

class ConsoleDataWriter extends DataWriter with FSM[DataWriterState, Unit, DataWriterMessage] with Logging {
  
	private var runningUsersCount = 0
	private var doneUsersCount = 0
	private var totalUsersCount = 0
	private var successfulRequestsCount = 0
	private var failedRequestsCount = 0
	private var startUpTime = 0L
	private var lastDisplayTime = 0L

	private val displayPeriod = 5 * 1000

	this.startWith(DataWriterUninitialized, Unit)
	when (DataWriterUninitialized){
		case Event(InitializeDataWriter(_, total, _, _), _) =>
			runningUsersCount = 0
			doneUsersCount = 0
			totalUsersCount = total
			successfulRequestsCount = 0
			failedRequestsCount = 0
			startUpTime = currentTimeMillis
			lastDisplayTime = currentTimeMillis
			goto(DataWriterInitialized)
		case unknown: DataWriterMessage =>
		  error("Unsupported DataWriterMessage" + unknown)
		  stay
//		case unknown: AnyRef => error("Unsupported message type in uninilialized state" + unknown.getClass)
//		case unknown: Any => error("Unsupported message type in uninilialized state " + unknown)
    }

	when (DataWriterInitialized) {
		case Event(RequestRecord(scenarioName, userId, actionName, executionStartDate, executionEndDate, requestSendingEndDate, responseReceivingStartDate, resultStatus, resultMessage, extraInfo), _) =>
			actionName match {
				case START_OF_SCENARIO => runningUsersCount += 1
				case END_OF_SCENARIO => runningUsersCount -= 1; doneUsersCount += 1
				case _ => resultStatus match {
					case OK => successfulRequestsCount += 1
					case KO => failedRequestsCount += 1
				}
			}

			val now = currentTimeMillis
			if (now - lastDisplayTime > displayPeriod) {
				lastDisplayTime = now
				val timeSinceStartUpInSec = (now - startUpTime) / 1000
				println(new StringBuilder()
					.append(timeSinceStartUpInSec)
					.append(" sec\tUsers: waiting=")
					.append(totalUsersCount - runningUsersCount - doneUsersCount)
					.append(" running=")
					.append(runningUsersCount)
					.append(" done=")
					.append(doneUsersCount)
					.append("\tRequests: OK=")
					.append(successfulRequestsCount)
					.append(" KO=")
					.append(failedRequestsCount))
			}
			stay

		case Event(FlushDataWriter, _) =>
		  goto(DataWriterUninitialized) // return to uninitialized state
		case unknown: DataWriterMessage =>
		  error("Unsupported DataWriterMessage" + unknown)
		  stay
//		case unknown: AnyRef => error("Unsupported message type in inilialized state " + unknown.getClass)
//		case unknown: Any => error("Unsupported message type in inilialized state " + unknown)
	}
}