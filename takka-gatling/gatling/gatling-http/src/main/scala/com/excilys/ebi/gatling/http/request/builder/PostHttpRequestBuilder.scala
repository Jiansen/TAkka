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
import com.excilys.ebi.gatling.http.request.HttpRequestBody
import com.ning.http.client.{ Realm, FilePart }

/**
 * This class defines an HTTP request with word POST in the DSL
 */
class PostHttpRequestBuilder(
	requestName: String,
	url: EvaluatableString,
	queryParams: List[HttpParam],
	params: List[HttpParam],
	headers: Map[String, EvaluatableString],
	body: Option[HttpRequestBody],
	uploadedFile: Option[FilePart],
	realm: Option[Session => Realm],
	checks: List[HttpCheck[_]])
		extends AbstractHttpRequestWithBodyAndParamsBuilder[PostHttpRequestBuilder](requestName, "POST", url, queryParams, params, headers, body, uploadedFile, realm, checks) {

	private[http] def newInstance(
		requestName: String,
		url: EvaluatableString,
		queryParams: List[HttpParam], params: List[HttpParam],
		headers: Map[String, EvaluatableString],
		body: Option[HttpRequestBody],
		uploadedFile: Option[FilePart],
		realm: Option[Session => Realm],
		checks: List[HttpCheck[_]]) = {
		new PostHttpRequestBuilder(requestName, url, queryParams, params, headers, body, uploadedFile, realm, checks)
	}
}
