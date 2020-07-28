package com.labs.http.sparkplug

import java.io.PrintWriter
import java.util

import com.labs.domain.models.Json
import com.labs.globals.LabsGlobal
import com.labs.storage.DataModel
import com.labs.utils.date.FormatDateTime
import org.joda.time.{DateTime, DateTimeZone}
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.JsonAST.{JObject, JString}

object RoutingResponseHandler {

	val VersionDate: DateTime = DateTime.now

	val GET = "GET"
	val POST = "POST"
	val PUT = "PUT"
	val DELETE = "DELETE"
	val TRACE = "Trace"

	//final val emitterRef: mutable.LinkedHashMap[String, AtomicReference[jetty.EventSource.Emitter]] = mutable.LinkedHashMap[String, AtomicReference[jetty.EventSource.Emitter]]()

	final val PathSplit = "/"

	/**
	  * The code of this method has been broken down into multiple iterators for efficiency, since it was showing as having high cpu usage in JProfiler
	  * @param accessPath
	  * @param pathFunc
	  * @param pathKeyMapReference
	  * @return
	  */
	def evaluatePathForKeys(accessPath: String, pathFunc: PathFunc, pathKeyMapReference: util.HashMap[String, util.HashMap[String, String]]): Boolean = {

		var pathSplit: Vector[String] = accessPath.split(PathSplit).toVector

		if( pathFunc.containsParameters && (pathSplit.length == pathFunc.routingSplit.size || pathFunc.endsWithWildCard) ) {
			val keyMap: util.HashMap[String, String] = new util.HashMap[String, String]

			val evaluatedRouteList = pathFunc.routingSplit.toSeq.map( pathPart =>

				if( pathPart.isWildcard ){

					val split = pathSplit.splitAt(pathPart.position)

					val starRoute: String = split._2.mkString(PathSplit)

					keyMap.put(pathPart.pathPart, starRoute)

					pathSplit = split._1 :+ starRoute
					Option(pathPart.pathPart)
				} else {

					val check = if( pathSplit.size-1 >= pathPart.position ){
						val extractedPart = pathSplit(pathPart.position)

						if( extractedPart == pathPart.pathPart )
							true
						else if( pathPart.pathPart != extractedPart && pathPart.isParameter )
							true
						else
							false
					}else
						false

					if ( pathPart.isParameter && check ) {
						keyMap.put(pathPart.pathPartNoColon, pathSplit(pathPart.position))
						Option(pathPart.pathPart)
					}
					else if ( check )
						Option(pathPart.pathPart)
					else
						None
				}
			)

			val evaluatedRoute = evaluatedRouteList.flatten

			if( evaluatedRoute.size == pathFunc.size && evaluatedRoute.size == pathSplit.length ){
				pathKeyMapReference.put(accessPath, keyMap)
				true
			}
			else false

		}
		else false

	}

	case class Chunker(writer: PrintWriter)(implicit request: HttpRequest) {
		var count = 0

		def writeStringPart(bodyParts: Seq[String]): Unit = {
			bodyParts.foreach{ part =>
				writer.write(part)
				writer.write("\015\012")
				writer.flush()
			}
			count=count+1
		}

		def close: Unit = writer.close()
	}

	private def metaDataHeader(implicit request: HttpRequest): Unit = {
		request.servletResponse.setHeader("x-elapsed", (DateTime.now.getMillis - request.start.getMillis).toString)
//		request.servletResponse.setHeader("x-totalresults",request.TotalResults.toString)
	}

	def chunkedResponse(setContentType:String)(implicit request: HttpRequest): Chunker = {

		metaDataHeader

		request.servletResponse.setContentType(setContentType)
		request.servletResponse.setCharacterEncoding("UTF-8")

		Chunker(request.servletResponse.getWriter)

	}

	def html[T](body: T)(implicit request: HttpRequest): Unit = {
		request.servletResponse.setContentType("text/html")
		defaultResponse(body)
	}

	def js[T](body: T)(implicit request: HttpRequest): Unit = {
		request.servletResponse.setContentType("text/javascript")
		request.servletResponse.setHeader("Last-Modified", FormatDateTime.getFriendlyDate(VersionDate, "EEE, dd MMM yyyy HH:mm:ss z"))
		request.servletResponse.setHeader("cache-control", "max-age=31536000, public")
		request.servletResponse.setHeader("expires", FormatDateTime.getFriendlyDate(VersionDate.plusYears(1), "EEE, dd MMM yyyy HH:mm:ss z"))

		defaultResponse(body)
	}

	def css[T](body: T)(implicit request: HttpRequest): Unit = {
		request.servletResponse.setContentType("text/css")
		request.servletResponse.setHeader("Last-Modified", FormatDateTime.getFriendlyDate(VersionDate, "EEE, dd MMM yyyy HH:mm:ss z"))
		request.servletResponse.setHeader("cache-control", "max-age=31536000, public")
		request.servletResponse.setHeader("expires", FormatDateTime.getFriendlyDate(VersionDate.plusYears(1), "EEE, dd MMM yyyy HH:mm:ss z"))
		defaultResponse(body)
	}

//	def json(body: String)(implicit request: HttpRequest): Unit = {
//
//		request.servletResponse.setContentType("application/json")
//		defaultResponse(body)
//	}

	private def defaultResponse[T](body: T)(implicit request: HttpRequest): Unit = {
		request.servletResponse.setCharacterEncoding("UTF-8")

		val bodyContent = body match{
			case str: String => str
			case _ => body.toString
		}

		request.servletResponse.getWriter.write(bodyContent)
	}

//	private def metaData()(implicit request: HttpRequest): JValue = {
//
//		JObject( List(
//			"time" -> JString(new DateTime(DateTimeZone.UTC).toString),
//			"elapsed" -> toJsonWithBrains(DateTime.now.getMillis - request.start.getMillis),
////			"totalresults" -> toJsonWithBrains(request.TotalResults),
//			"skip" -> toJsonWithBrains(request.Skip),
//			"limit" -> toJsonWithBrains(request.Limit ))
//		)
//	}

//	def json[T](body: T)(implicit request: HttpRequest): Unit = {
//		val jValueObject: JValue = toJsonWithBrains(body)
//
//		request.servletResponse.setContentType("application/json")
//		request.servletResponse.setCharacterEncoding("UTF-8")
//
//		request.servletResponse.getWriter.write( Json.fromJsonToString(
//			JObject(List(
//				"meta" -> metaData,
//				"data" -> jValueObject
//			))
//		))
//	}

//	def toJsonWithBrains[T](a:T): JValue = a match {
//		case seq: Seq[_] => seq.map( model => model match {
//			case aa: DataModel[_] => aa.toJson
//			case aa: JObject      => aa
//			case aa: AnyRef => toJsonWithBrains(aa)
//		})
//		case value: DataModel[_]  => value.toJson
//		case _ => Json.toJsValue(a.asInstanceOf[AnyRef])
//	}
}
