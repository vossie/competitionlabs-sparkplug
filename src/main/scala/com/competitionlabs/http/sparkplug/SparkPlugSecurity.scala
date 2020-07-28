package com.labs.http.sparkplug

import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

trait SparkPlugSecurity extends SparkPlugResponses {

	def onRequest(pathKey: PathKey, path: String, pathKeyValues: Map[String, String], allPathKeys: List[PathKey])(onAccessGranted: HttpRequest => Unit)(implicit req: HttpServletRequest, resp: HttpServletResponse): Unit
}
