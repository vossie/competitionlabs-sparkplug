package com.labs.http.sparkplug

trait SparkPlugViews {

	val templateFolder: Option[String]

	def render(viewName: String, params: Map[String, String] = Map.empty): String
}
