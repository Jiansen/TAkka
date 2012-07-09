/**
 * Copyright 2011-2012 eBusiness Information, Groupe Excilys (www.excilys.com)
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
package com.excilys.ebi.gatling.core.result.reader

import org.joda.time.DateTime

import com.excilys.ebi.gatling.core.result.message.RequestStatus.RequestStatus

object ChartRequestRecord {

	def apply(scenarioName: String, userId: Int, requestName: String, executionStartDate: Long, executionEndDate: Long, requestSendingEndDate: Long, responseReceivingStartDate: Long, requestStatus: RequestStatus) = {

		val responseTime = executionEndDate - executionStartDate
		val latency = responseReceivingStartDate - requestSendingEndDate
		val executionStartDateNoMillis = new DateTime(executionStartDate).withMillisOfSecond(0).getMillis
		val executionEndDateNoMillis = new DateTime(executionEndDate).withMillisOfSecond(0).getMillis

		new ChartRequestRecord(
			scenarioName.intern,
			userId,
			requestName.intern,
			executionStartDateNoMillis,
			executionEndDateNoMillis,
			responseTime,
			latency,
			requestStatus)
	}
}

class ChartRequestRecord(
	val scenarioName: String,
	val userId: Int,
	val requestName: String,
	val executionStartDateNoMillis: Long,
	val executionEndDateNoMillis: Long,
	val responseTime: Long,
	val latency: Long,
	val requestStatus: RequestStatus) {

	override def equals(other: Any) =
		other match {
			case that: ChartRequestRecord =>
				(that canEqual this) &&
					scenarioName == that.scenarioName &&
					userId == that.userId &&
					requestName == that.requestName &&
					executionStartDateNoMillis == that.executionStartDateNoMillis &&
					executionEndDateNoMillis == that.executionEndDateNoMillis &&
					responseTime == that.responseTime &&
					latency == that.latency &&
					requestStatus == that.requestStatus
			case _ => false
		}

	def canEqual(other: Any): Boolean =
		other.isInstanceOf[ChartRequestRecord]

	override def hashCode = {
		41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 + scenarioName.hashCode) + userId.hashCode) + requestName.hashCode) + executionStartDateNoMillis.hashCode) + executionEndDateNoMillis.hashCode) + responseTime.hashCode) + latency.hashCode) + requestStatus.hashCode
	}

}
