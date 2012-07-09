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
package com.excilys.ebi.gatling.core.check.extractor.css

import scala.collection.JavaConversions.asScalaBuffer

import com.excilys.ebi.gatling.core.check.extractor.Extractor.{ toOption, seqToOption }

import jodd.lagarto.dom.{ NodeSelector, LagartoDOMBuilder }

object CssExtractor {

	/**
	 * The DOM Builder singleton
	 */
	val domBuilder = new LagartoDOMBuilder
}

/**
 * A built-in extractor for extracting values with Css Selectors
 *
 * @constructor creates a new CssExtractor
 * @param text the text where the search will be made
 */
class CssExtractor(text: String) {

	val selector = new NodeSelector(CssExtractor.domBuilder.parse(text))

	/**
	 * @param expression a String containing the CSS selector
	 * @return an option containing the value if found, None otherwise
	 */
	def extractOne(occurrence: Int)(expression: String): Option[String] = extractMultiple(expression) match {
		case Some(results) if (results.isDefinedAt(occurrence)) => results(occurrence)
		case _ => None
	}

	/**
	 * @param expression a String containing the CSS selector
	 * @return an option containing the values if found, None otherwise
	 */
	def extractMultiple(expression: String): Option[Seq[String]] = selector.select(expression).map(_.getTextContent.trim())

	/**
	 * @param expression a String containing the CSS selector
	 * @return an option containing the number of values if found, None otherwise
	 */
	def count(expression: String): Option[Int] = extractMultiple(expression).map(_.size)

}