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
package com.excilys.ebi.gatling.charts.config

import scala.tools.nsc.io.Path.string2path

import com.excilys.ebi.gatling.core.config.GatlingFiles.{ resultFolder, GATLING_JS }
import com.excilys.ebi.gatling.core.util.FileHelper.{ formatToFilename, HTML_EXTENSION }

object ChartsFiles {
	val JQUERY_FILE = "jquery.min.js"
	val MENU_FILE = "menu.js"
	val ALL_SESSIONS_FILE = "all_sessions.js"
	val STATS_JS_FILE = "stats.js"
	val STATS_TSV_FILE = "stats.tsv"
	val GLOBAL_PAGE_NAME = "Global Information"

	val GATLING_TEMPLATE = "templates/"
	val GATLING_TEMPLATE_STATISTICS_COMPONENT_URL = GATLING_TEMPLATE + "statistics_component.html.ssp"
	val GATLING_TEMPLATE_LAYOUT_FILE_URL = GATLING_TEMPLATE + "page_layout.html.ssp"
	val GATLING_TEMPLATE_ALL_SESSIONS_JS_FILE_URL = GATLING_TEMPLATE + "all_sessions.js.ssp"
	val GATLING_TEMPLATE_MENU_JS_FILE_URL = GATLING_TEMPLATE + "menu.js.ssp"
	val GATLING_TEMPLATE_STATS_JS_FILE_URL = GATLING_TEMPLATE + "stats.js.ssp"
	val GATLING_TEMPLATE_STATS_TSV_FILE_URL = GATLING_TEMPLATE + "stats.tsv.ssp"

	def menuFile(runOn: String) = resultFolder(runOn) / GATLING_JS / MENU_FILE
	def allSessionsFile(runOn: String) = resultFolder(runOn) / GATLING_JS / ALL_SESSIONS_FILE
	def globalFile(runOn: String) = resultFolder(runOn) / "index.html"

	def requestFile(runOn: String, requestName: String) = resultFolder(runOn) / (formatToFilename(requestName) + HTML_EXTENSION)
	def jsStatsFile(runOn: String) = resultFolder(runOn) / GATLING_JS / STATS_JS_FILE
	def tsvStatsFile(runOn: String) = resultFolder(runOn) / STATS_TSV_FILE
}