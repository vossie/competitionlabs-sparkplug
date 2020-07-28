package com.labs.http.sparkplug.impl

import com.labs.http.sparkplug.{SparkPlugJson, SparkPlugMarshaller, SparkPlugViews}

class SparkPlugDefaultMarshaller extends SparkPlugMarshaller {

	override val json: SparkPlugJson  = new SparkPlugJson

	override val views: SparkPlugViews = new SparkPlugViews {
		override val templateFolder: Option[String] = None

		override def render(viewName: String, params: Map[String, String]): String =
			throw new NotImplementedError("Views engine not implemented.")
	}
}

