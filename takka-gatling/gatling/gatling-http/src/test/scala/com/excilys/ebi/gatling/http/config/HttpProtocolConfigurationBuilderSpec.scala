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
package com.excilys.ebi.gatling.http.config

import org.specs2.mutable.Specification
import com.ning.http.client.{Response, Request}
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HttpProtocolConfigurationBuilderSpec extends Specification {

  "http protocol configuration builder" should {
    "support an optional extra request info extractor" in {

      val expectedExtractor: (Request => List[String]) = (Request) => Nil

      val builder = HttpProtocolConfigurationBuilder.httpConfig
        .disableWarmUp
        .requestInfoExtractor(expectedExtractor)
      val config: HttpProtocolConfiguration = builder.build

      config.extraRequestInfoExtractor.get should beEqualTo(expectedExtractor)
    }

    "support an optional extra response info extractor" in {

      val expectedExtractor: (Response => List[String]) = (Response) => Nil

      val builder = HttpProtocolConfigurationBuilder.httpConfig
        .disableWarmUp
        .responseInfoExtractor(expectedExtractor)
      val config: HttpProtocolConfiguration = builder.build

      config.extraResponseInfoExtractor.get should beEqualTo(expectedExtractor)
    }
  }
}
