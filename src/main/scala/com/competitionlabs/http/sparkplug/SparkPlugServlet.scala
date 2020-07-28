package com.labs.http.sparkplug

import java.util

import com.competitionlabs.http.{LabsHttpServlet, WebServer}
import com.labs.globals.LabsGlobal
import com.labs.http._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import scala.language.implicitConversions
import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

trait SparkPlugServlet extends LabsHttpServlet {

	val routes: Routes
	val webServer: WebServer
	val global: LabsGlobal

	val marshaller: SparkPlugMarshaller
	val securityContext: SparkPlugSecurity

	override def Get(implicit req: HttpServletRequest, resp: HttpServletResponse): Unit = apply(routes.getMethods)

	override def Post(implicit req: HttpServletRequest, resp: HttpServletResponse): Unit = apply(routes.postMethods)

	override def Put(implicit req: HttpServletRequest, resp: HttpServletResponse): Unit = apply(routes.putMethods)

	override def Delete(implicit req: HttpServletRequest, resp: HttpServletResponse): Unit = apply(routes.deleteMethods)

	def apply(routesByMethod: List[PathFunc])(implicit req: HttpServletRequest, resp: HttpServletResponse): Unit = {

		val path: Path = req.getRequestURI
		val allPathKeyValues: util.HashMap[String, util.HashMap[String, String]] = new util.HashMap[String, util.HashMap[String, String]]

		try {

			var filteredPath: Option[Route] = None

			val httpMethod = req.getMethod.toUpperCase

			val iterator = routesByMethod.toIterator

			while (iterator.hasNext && filteredPath.isEmpty){

				val route = iterator.next()

				if (route.pathKey.path == path) {
					filteredPath = Option(Route(route.pathKey.path, httpMethod, route))
				}
				else {

					if (route.endsWithWildCard && path.startsWith(route.starLookup.head)) {
						val lookupPath = path.split(route.starLookup.head).last
						val newMap = new util.HashMap[String, String]()
						newMap.put("*", lookupPath)
						allPathKeyValues.put( lookupPath, newMap )
						filteredPath = Option(Route(lookupPath, httpMethod, route))
					} else {
						if (RoutingResponseHandler.evaluatePathForKeys(path, route, allPathKeyValues)) {
							filteredPath = Option(Route(path.split(route.starLookup.head).last, httpMethod, route))
						}
					}
				}
			}

			filteredPath match {
				case Some(routeController) =>
					val pathParams = allPathKeyValues.getOrDefault(routeController.path, new util.HashMap[String, String]).toMap
					securityContext.onRequest(routeController.pathFunc.pathKey, routeController.path, pathParams, routes.allPathKeys)(onAccessGranted = request =>
						routeController.pathFunc.func(request)
					)

					case _ =>
						NotFound()(req, resp)
			}
		}
		catch {
			case e: Throwable =>
				logger.error(path, e)
				throw e
		}
	}
}
