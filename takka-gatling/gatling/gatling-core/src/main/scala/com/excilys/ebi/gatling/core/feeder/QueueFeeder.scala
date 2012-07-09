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
package com.excilys.ebi.gatling.core.feeder

import java.util.concurrent.ConcurrentLinkedQueue

import scala.collection.JavaConverters.seqAsJavaListConverter

import com.excilys.ebi.gatling.core.action.system

import grizzled.slf4j.Logging

class QueueFeeder(feederSource: FeederSource) extends Feeder with Logging {

	private val values = new ConcurrentLinkedQueue(feederSource.values.asJava)

	def next: Map[String, String] = Option(values.poll).getOrElse {
		error("There are not enough records in the feeder '" + feederSource.name + "'.\nPlease add records or use another feeder strategy.\nStopping simulation here...")
		system.shutdown
		sys.exit
	}
}