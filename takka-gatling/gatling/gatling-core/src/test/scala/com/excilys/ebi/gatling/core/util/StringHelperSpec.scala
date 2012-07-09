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
package com.excilys.ebi.gatling.core.util

import java.io.File
import java.security.MessageDigest

import org.apache.commons.io.FileUtils
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import com.excilys.ebi.gatling.core.session.Session

@RunWith(classOf[JUnitRunner])
class StringHelperSpec extends Specification {

	val fileBytes = FileUtils.readFileToByteArray(new File("src/test/resources/gatling-core-1.2.1.pom"))

	"parseEvaluatable" should {

		"return expected result with 1 monovalued expression that is the whole string" in {
			val session = new Session("scenario", 1, Map("bar" -> "BAR"))
			StringHelper.parseEvaluatable("${bar}")(session) must beEqualTo("BAR")
		}

		"return expected result with 1 monovalued expression at the end of the string" in {
			val session = new Session("scenario", 1, Map("bar" -> "BAR"))
			StringHelper.parseEvaluatable("foo${bar}")(session) must beEqualTo("fooBAR")
		}

		"return expected result with 1 monovalued expression at the beginning of the string" in {
			val session = new Session("scenario", 1, Map("bar" -> "BAR"))
			StringHelper.parseEvaluatable("${bar}baz")(session) must beEqualTo("BARbaz")
		}

		"return expected result with 1 monovalued expression in the middle of the string" in {
			val session = new Session("scenario", 1, Map("bar" -> "BAR"))
			StringHelper.parseEvaluatable("foo${bar}baz")(session) must beEqualTo("fooBARbaz")
		}

		"return expected result with 2 monovalued expressions" in {
			val session = new Session("scenario", 1, Map("foo" -> "FOO", "bar" -> "BAR"))
			StringHelper.parseEvaluatable("${foo} ${bar}")(session) must beEqualTo("FOO BAR")
		}

		"handle gracefully monovalued expression with missing attribute" in {
			val session = new Session("scenario", 1, Map.empty)
			StringHelper.parseEvaluatable("foo${bar}")(session) must beEqualTo("foo")
		}

		"return expected result with multivalued expression with static index" in {
			val session = new Session("scenario", 1, Map("bar" -> List("BAR1", "BAR2")))
			StringHelper.parseEvaluatable("foo${bar(1)}")(session) must beEqualTo("fooBAR2")
		}

		"return expected result with multivalued expression with resolved index" in {
			val session = new Session("scenario", 1, Map("bar" -> List("BAR1", "BAR2"), "baz" -> 1))
			StringHelper.parseEvaluatable("{foo${bar(baz)}}")(session) must beEqualTo("{fooBAR2}")
		}

		"handle gracefully multivalued expression with static index and missing attribute" in {
			val session = new Session("scenario", 1, Map.empty)
			StringHelper.parseEvaluatable("foo${bar(1)}")(session) must beEqualTo("foo")
		}

		"handle gracefully multivalued expression with static index and empty attribute" in {
			val session = new Session("scenario", 1, Map("bar" -> Nil))
			StringHelper.parseEvaluatable("foo${bar(1)}")(session) must beEqualTo("foo")
		}

		"handle gracefully multivalued expression with static index and missing index" in {
			val session = new Session("scenario", 1, Map("bar" -> List("BAR1")))
			StringHelper.parseEvaluatable("foo${bar(1)}")(session) must beEqualTo("foo")
		}

		"handle gracefully multivalued expression with missing resolved index attribute" in {
			val session = new Session("scenario", 1, Map("bar" -> List("BAR1", "BAR2")))
			StringHelper.parseEvaluatable("{foo${bar(baz)}}")(session) must beEqualTo("{foo}")
		}
	}

	"bytes2Hex" should {

		"correctly compute file sha-1" in {
			val md = MessageDigest.getInstance("SHA-1")
			md.update(fileBytes)
			val digestBytes = md.digest
			StringHelper.bytes2Hex(digestBytes) must beEqualTo("aefb180cba67752f54c7eacf45356dd55db4dcc4")
		}

		"correctly compute file md5" in {
			val md = MessageDigest.getInstance("MD5")
			md.update(fileBytes)
			val digestBytes = md.digest
			StringHelper.bytes2Hex(digestBytes) must beEqualTo("694ad6ef693b035d5207506efa2a6d39")
		}
	}
}