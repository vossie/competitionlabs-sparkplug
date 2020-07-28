package com.labs.http

import java.util

import com.labs.domain.models.FormFile
import com.labs.globals.LabsGlobal
import com.labs.identity.SessionData
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import scala.collection.immutable.ListMap

package object sparkplug {

	final val WebServerType = "CompetitionLabs"

	type KeyMap = util.HashMap[String, String]
	type RouteFunc = HttpRequest => Unit
	type HttpRequestMethod = String
	type Path = String
	type RoutesList = ListMap[PathKey, RouteFunc]

	case class PathKey(method: HttpRequestMethod, path:Path, invokerPackage: String, description: String, servers: List[String], classVarName: String, operationId: String){
		def name: String = operationId
		def displayName: String = operationId
		val resourceType: String = "urn:CompetitionLabs:" + classVarName
		def uri: String = path
		val resourceIdentifier: String = s"$invokerPackage.$classVarName.$operationId"
	}

	case class PathFunc(pathKey:PathKey, func: RouteFunc) {

		final val starLookup: Vector[Path] = pathKey.path.split("\\*").toVector

		final val pathSplit: Vector[Path] = pathKey.path.split("/").toVector

		final val containsParameters = pathKey.path.contains(":")

		final val pathHead: String = if(pathSplit.nonEmpty) pathSplit.head else ""

		final val endsWithWildCard: Boolean = pathKey.path.endsWith("*")

		final val routingSplit: Set[PathEntry] = pathSplit.zipWithIndex.map(x => PathEntry(x._2, x._1)).toSet[PathEntry]

		final val size = routingSplit.size

		final val keyMap: Map[Path, Int] = routingSplit.flatMap( pathEntry =>
			if(pathEntry.isParameter) Option(pathEntry.pathPartNoColon -> pathEntry.position)
			else None
		).toMap
	}

	case class Route(path: Path, method: HttpRequestMethod, pathFunc: PathFunc)

	case class PathEntry(position: Int, pathPart: String) {
		final val isParameter: Boolean = pathPart.startsWith(":")
		final val isWildcard: Boolean = pathPart == "*"
		final val pathPartNoColon = pathPart.replace(":","")
	}

	case class HttpRequest(override val path: String, override val pathKeyValues: Map[String, String], servletRequest: HttpServletRequest,
						   servletResponse: HttpServletResponse, session: Map[String,Any], global: LabsGlobal, pathKey: PathKey, allPathKeys: List[PathKey]) extends HttpRequestHelpers {
		servletResponse.setHeader("Server", WebServerType)
		servletResponse.setHeader("Connection", "keep-alive")
	}

	case class FormData(formFields: util.HashMap[String, String] = new util.HashMap[String, String](), files: Seq[(String, FormFile)] = Seq.empty){
		def getField(key: String): Option[String] = Option(formFields.get(key)) match {
			case Some(s) => if(s.isEmpty) None else Option(s)
			case _ => None
		}
	}
}
