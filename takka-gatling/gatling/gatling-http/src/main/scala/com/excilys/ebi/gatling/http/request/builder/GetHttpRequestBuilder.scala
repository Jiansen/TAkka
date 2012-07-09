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
package com.excilys.ebi.gatling.http.request.builder

import com.excilys.ebi.gatling.core.session.{ Session, EvaluatableString }
import com.excilys.ebi.gatling.http.check.HttpCheck
import com.ning.http.client.Realm

/**
 * This class defines an HTTP request with word GET in the DSL
 */
class GetHttpRequestBuilder(
	requestName: String,
	url: EvaluatableString,
	queryParams: List[HttpParam],
	headers: Map[String, EvaluatableString],
	realm: Option[Session => Realm],
	checks: List[HttpCheck[_]])
		extends AbstractHttpRequestBuilder[GetHttpRequestBuilder](requestName, "GET", url, queryParams, headers, realm, checks) {

	private[http] def newInstance(
		requestName: String,
		url: EvaluatableString,
		queryParams: List[HttpParam],
		headers: Map[String, EvaluatableString],
		realm: Option[Session => Realm],
		checks: List[HttpCheck[_]]) = {
		new GetHttpRequestBuilder(requestName, url, queryParams, headers, realm, checks)
	}
}