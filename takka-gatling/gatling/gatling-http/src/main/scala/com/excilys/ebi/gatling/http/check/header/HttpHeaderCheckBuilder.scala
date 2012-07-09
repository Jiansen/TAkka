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
package com.excilys.ebi.gatling.http.check.header

import java.net.URLDecoder

import scala.collection.JavaConversions.asScalaBuffer

import com.excilys.ebi.gatling.core.check.ExtractorFactory
import com.excilys.ebi.gatling.core.check.extractor.Extractor.{ toOption, seqToOption }
import com.excilys.ebi.gatling.core.config.GatlingConfiguration.configuration
import com.excilys.ebi.gatling.core.session.EvaluatableString
import com.excilys.ebi.gatling.http.Headers
import com.excilys.ebi.gatling.http.check.HttpMultipleCheckBuilder
import com.excilys.ebi.gatling.http.request.HttpPhase.HeadersReceived
import com.excilys.ebi.gatling.http.response.ExtendedResponse

/**
 * HttpHeaderCheckBuilder class companion
 *
 * It contains DSL definitions
 */
object HttpHeaderCheckBuilder {

	private def findExtractorFactory(occurrence: Int): ExtractorFactory[ExtendedResponse, String, String] = (response: ExtendedResponse) => (headerName: String) => {
		val headers = response.getHeaders(headerName)
		if (headers.size > occurrence) {
			val headerValue = headers.get(occurrence)
			if (headerName == Headers.Names.LOCATION)
				URLDecoder.decode(headerValue, configuration.encoding)
			else
				headerValue

		} else
			None
	}

	private val findAllExtractorFactory: ExtractorFactory[ExtendedResponse, String, Seq[String]] = (response: ExtendedResponse) => (headerName: String) => {

		val headerValues = response.getHeaders(headerName)
		if (headerName == Headers.Names.LOCATION)
			headerValues.map(URLDecoder.decode(_, configuration.encoding))
		else
			headerValues.toSeq
	}

	private val countExtractorFactory: ExtractorFactory[ExtendedResponse, String, Int] = (response: ExtendedResponse) => (headerName: String) => toOption(response.getHeaders(headerName).size)

	/**
	 * Will check the value of the header in the session
	 *
	 * @param headerName the function returning the name of the header
	 */
	def header(headerName: EvaluatableString) = new HttpMultipleCheckBuilder(findExtractorFactory, findAllExtractorFactory, countExtractorFactory, headerName, HeadersReceived)
}