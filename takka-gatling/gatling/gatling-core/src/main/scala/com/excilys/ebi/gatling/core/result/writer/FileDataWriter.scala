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
/**
 * takka-galting is a deravative work of the galting project, developed by eBusiness Information, Groupe Excilys (www.excilys.com)
 */
package com.excilys.ebi.gatling.core.result.writer

import java.io.{ OutputStreamWriter, FileOutputStream, BufferedOutputStream }
import java.util.concurrent.CountDownLatch

import com.excilys.ebi.gatling.core.config.GatlingFiles.simulationLogDirectory
import com.excilys.ebi.gatling.core.result.message.{ InitializeDataWriter, FlushDataWriter, RequestRecord }
import com.excilys.ebi.gatling.core.result.message.RecordType.{ RUN, ACTION }
import com.excilys.ebi.gatling.core.util.DateHelper.toTimestamp
import com.excilys.ebi.gatling.core.util.FileHelper.TABULATION_SEPARATOR
import com.excilys.ebi.gatling.core.util.StringHelper.END_OF_LINE

import grizzled.slf4j.Logging

import takka.actor.FSM
import com.excilys.ebi.gatling.core.result.message.DataWriterMessage

object FileDataWriter {

	val emptyField = " "

	val sanitizerPattern = """[\n\r\t]""".r

	private[writer] def append(appendable: Appendable, requestRecord: RequestRecord) {

		appendable.append(ACTION).append(TABULATION_SEPARATOR)
			.append(requestRecord.scenarioName).append(TABULATION_SEPARATOR)
			.append(requestRecord.userId.toString).append(TABULATION_SEPARATOR)
			.append(requestRecord.requestName).append(TABULATION_SEPARATOR)
			.append(requestRecord.executionStartDate.toString).append(TABULATION_SEPARATOR)
			.append(requestRecord.executionEndDate.toString).append(TABULATION_SEPARATOR)
			.append(requestRecord.requestSendingEndDate.toString).append(TABULATION_SEPARATOR)
			.append(requestRecord.responseReceivingStartDate.toString).append(TABULATION_SEPARATOR)
			.append(requestRecord.requestStatus.toString).append(TABULATION_SEPARATOR)
			.append(requestRecord.requestMessage.getOrElse(emptyField))

		requestRecord.extraInfo.foreach((info: String) => {
			appendable.append(TABULATION_SEPARATOR).append(sanitize(info))
		})

		appendable.append(END_OF_LINE)
	}

	/**
	 * Converts whitespace characters that would break the simulation log format into spaces.
	 * @param input
	 * @return
	 */
	private[writer] def sanitize(input: String): String = {
		sanitizerPattern.replaceAllIn(input, " ")
	}
}

/**
 * File implementation of the DataWriter
 *
 * It writes the data of the simulation if a tabulation separated values file
 */
class FileDataWriter extends DataWriter with FSM[DataWriterState, Unit, DataWriterMessage] with Logging {
	/**
	 * The OutputStreamWriter used to write to files
	 */
	private var osw: OutputStreamWriter = _

	/**
	 * The countdown latch that will be decreased when all message are written and all scenarios ended
	 */
	private var latch: CountDownLatch = _

	this.startWith(DataWriterUninitialized, Unit)
	when(DataWriterUninitialized){
		case Event(InitializeDataWriter(runRecord, totalUsersCount, latch, encoding), _) =>
			this.latch = latch
			val simulationLog = simulationLogDirectory(runRecord.runUuid) / "simulation.log"
			osw = new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(simulationLog.toString)), encoding)
			osw.append(RUN).append(TABULATION_SEPARATOR)
				.append(toTimestamp(runRecord.runDate)).append(TABULATION_SEPARATOR)
				.append(runRecord.runId).append(TABULATION_SEPARATOR)
				// hack for being able to deserialize in FileDataReader
				.append(if (runRecord.runDescription.isEmpty) FileDataWriter.emptyField else runRecord.runDescription)
				.append(END_OF_LINE)
			goto(DataWriterInitialized)
		case unknown: DataWriterMessage =>
		  error("Unsupported DataWriterMessage" + unknown)
		  stay

//		case unknown: AnyRef => error("Unsupported message type in uninilialized state" + unknown.getClass)
//		case unknown: Any => error("Unsupported message type in uninilialized state " + unknown)
	}

	when(DataWriterInitialized){
		case Event(requestRecord: RequestRecord, _) =>
			FileDataWriter.append(osw, requestRecord)
			stay
		case Event(FlushDataWriter, _) =>
			info("Received flush order")
			try {
				osw.flush
				stay
			} finally {
				// Decrease the latch (should be at 0 here)
				osw.close
				latch.countDown			  
				goto(DataWriterUninitialized) // return to uninitialized state
			}
		case unknown: DataWriterMessage =>
		  error("Unsupported DataWriterMessage" + unknown)
		  stay
//		case unknown: AnyRef => error("Unsupported message type in inilialized state " + unknown.getClass)
//		case unknown: Any => error("Unsupported message type in inilialized state " + unknown)
	}
}
