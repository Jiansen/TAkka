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
package com.excilys.ebi.gatling.app

object Options {
	val DEFAULT_RUN_ID = "run"
}

case class Options(
	var reportsOnlyFolder: Option[String] = None,
	var noReports: Boolean = false,
	var configFileName: Option[String] = None,
	var resultsFolder: Option[String] = None,
	var dataFolder: Option[String] = None,
	var requestBodiesFolder: Option[String] = None,
	var simulationSourcesFolder: Option[String] = None,
	var simulationBinariesFolder: Option[String] = None,
	var simulations: Option[List[String]] = None,
	var runName: String = Options.DEFAULT_RUN_ID)