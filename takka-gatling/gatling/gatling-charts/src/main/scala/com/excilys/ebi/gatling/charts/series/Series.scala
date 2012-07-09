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
package com.excilys.ebi.gatling.charts.series

import com.excilys.ebi.gatling.core.config.GatlingConfiguration.configuration

class Series[X, Y](val name: String, val data: Seq[(X, Y)], val colors: List[String]) {

	def sample: Seq[(X, Y)] = {
		val nbMax = configuration.chartingMaxPlotPerSerie
		val nb = data.size
		if (nb <= nbMax)
			data
		else {
			var i = 0
			data.filter { plot =>
				i = i + 1
				isPlotMandatory(plot) || i % (nb / nbMax) == 0
			}
		}
	}

	def isPlotMandatory(plot: (X, Y)) = false
}