package com.labs.http.sparkplug.impl

import com.labs.globals.LabsGlobal
import com.labs.http.sparkplug.{HttpRequest, PathKey, Routes, SparkPlugSecurity}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

class SparkPlugDefaultSecurity(implicit global: LabsGlobal) extends SparkPlugSecurity {

	def onRequest(pathKey: PathKey, path: String, pathKeyValues: Map[String, String], allPathKeys: List[PathKey])(onAccessGranted: HttpRequest => Unit)
				 (implicit req: HttpServletRequest, resp: HttpServletResponse): Unit = {

		onAccessGranted(
			HttpRequest(path, pathKeyValues, req, resp, Map.empty, global, pathKey, allPathKeys)
		)
	}
}
