package com.labs.http.sparkplug

import org.eclipse.jetty.http.HttpMethod

import scala.collection.immutable.ListMap
import scala.language.implicitConversions

trait Routes {

	private implicit def listMapToList(x: ListMap[PathKey, RouteFunc]): List[PathFunc] = x.map(tupleToPathFunc)
	private implicit def tupleToPathFunc(x: (PathKey, RouteFunc)): PathFunc = PathFunc(x._1,x._2)
	private implicit def iterableToList[T](x: Iterable[T]): List[T] = x.toList

	val routes: RoutesList

	final val GET = HttpMethod.GET.name()
	final val HEAD = HttpMethod.HEAD.name()
	final val POST = HttpMethod.POST.name()
	final val PUT = HttpMethod.PUT.name()
	final val DELETE = HttpMethod.DELETE.name()
	final val CONNECT = HttpMethod.CONNECT.name()
	final val OPTIONS = HttpMethod.OPTIONS.name()
	final val TRACE = HttpMethod.TRACE.name()

	lazy val byMethod: Map[HttpRequestMethod, ListMap[PathKey, RouteFunc]] = routes.groupBy(_._1.method)

	lazy val allPathKeys: List[PathKey] = routes.keys.toList
	lazy val getMethods: List[PathFunc] = routes.filter(_._1.method == GET)
	lazy val headMethods: List[PathFunc] = routes.filter(_._1.method == HEAD)
	lazy val postMethods: List[PathFunc] = routes.filter(_._1.method == POST)
	lazy val putMethods: List[PathFunc] = routes.filter(_._1.method == PUT)
	lazy val deleteMethods: List[PathFunc] = routes.filter(_._1.method == DELETE)
	lazy val connectMethods: List[PathFunc] = routes.filter(_._1.method == CONNECT)
	lazy val optionsMethods: List[PathFunc] = routes.filter(_._1.method == OPTIONS)
	lazy val traceMethods: List[PathFunc] = routes.filter(_._1.method == TRACE)

	def getRoutesByMethod(method:String): List[PathFunc] = method match {
		case GET => getMethods
		case HEAD => headMethods
		case POST => postMethods
		case PUT => putMethods
		case DELETE => deleteMethods
		case CONNECT => connectMethods
		case OPTIONS => optionsMethods
		case TRACE => traceMethods
	}
}
