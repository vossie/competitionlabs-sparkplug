package com.labs.http.sparkplug

import java.io.{BufferedReader, File, InputStreamReader}
import java.util
import java.util.zip.GZIPInputStream

import com.labs.domain.models.{AccountId, FormFile, QueryField, QueryOperator, QueryParam, _Language}
import com.labs.errors.AppError
import com.labs.errors.CommonErrors.ValidationFailed
import com.labs.storage.elasticsearch.impl.DataStoreCore
import com.labs.utils.StringUtils
import com.labs.utils.logging.LabsLogger
import javax.servlet.http.{Cookie, HttpServletRequest}
import org.apache.commons.fileupload.disk.DiskFileItemFactory
import org.apache.commons.fileupload.servlet.ServletFileUpload
import org.eclipse.jetty.server.Request
import org.joda.time.DateTime

import scala.collection.mutable

trait HttpRequestHelpers extends LabsLogger {

	val servletRequest: HttpServletRequest
	val path: String
	var sseRequest: Option[((String, AccountId, Int) => (HttpRequest) => Unit)] = None
	val pathKeyValues: Map[String, String]

	lazy val headers: HttpHeaders = {
		val it = servletRequest.getHeaderNames
		val map = new mutable.HashMap[String, String]()
		while( it.hasMoreElements ){
			val header = it.nextElement()

			map.put(header.toLowerCase, Option(servletRequest.getHeader(header)).getOrElse(""))
		}

		new HttpHeaders(map)
	}

	lazy val cookies: Map[String, Cookie] = servletRequest.getCookies.map( x => x.getName.toLowerCase -> x).toMap

	lazy val isGzip: Boolean = {
		val ce = headers.get("Content-Encoding")
		if( ce.nonEmpty && ce.get.contains("gzip") )
			true
		else
			false
	}

	val isJson: Boolean = {
		val ce = headers.get("Content-Type")
		if( ce.nonEmpty && ce.get.equals("application/json") )
			true
		else
			false
	}

	lazy val getContentType: Option[String] = Option(servletRequest.getContentType)

	lazy val queryParameters: Map[String, String] = {
		val queryString: Map[String, String] = Option(servletRequest.getQueryString).getOrElse("").split("&").flatMap{
			str =>
				val part = str.indexOf("=")
				val right = str.substring(part+1)
				val left = str.slice(0, part)

				if( right.nonEmpty ){
					Option(StringUtils.decode(left) -> StringUtils.decode(right))
				}else{
					None
				}
		}.toMap

		queryString
	}

	private lazy val specialCharacters = Seq(">","<","!","=", ".", "_")
	private lazy val validOptions = Map("._must=" -> "==", "._not=" -> "!=", "._should=" -> "=", "._gte=" -> ">==", "._gt=" -> ">=", "._lte=" -> "<==", "._lt=" -> "<=", "._minmatch=" -> "minShouldMatch")

	lazy val searchParameters: Map[QueryField, Map[QueryOperator, Array[QueryParam]]] = {
		val queryString = StringUtils.decode(Option(servletRequest.getQueryString).getOrElse(""))

		assert(queryString.length <= 64000, s"Maximum length exceeded for URL string.")


		val s = queryString.split("&").map { e => e.splitAt(e.lastIndexOf("=") + 1) }
		val r = s.filter(_._1.nonEmpty).map { e =>

			def isMatch(i: Int): Boolean = {
				if (i >= e._1.length) false // make it safe
				else specialCharacters.contains(e._1(i).toString)
			}

			def finEnd(i: Int): Int =
				if (i >= e._1.length)
					i
				else if (isMatch(i) && isMatch(i + 1))
					i
				else if (isMatch(i) && i == e._1.length - 1)
					i
				else
					finEnd(i + 1)

			val splitIndexAt = finEnd(0)

			val end = e._1.splitAt(splitIndexAt)

			(end._1, validOptions.getOrElse(end._2.toLowerCase(), end._2), e._2)
		}

		val k = r.groupBy(_._1).map { d =>
			val g = (d._1, d._2.map { k =>
				(k._2, k._3.split(","))
			})

			val g1 = g._2.groupBy(_._1)

			d._1 -> g1.map { g2 =>
				(g2._1, g2._2.flatMap(_._2))
			}
		}

		k

	}

	def getPassedSessionParameters(key: String): Option[String] = {
		val session = Option(servletRequest.getSession(false))
		if( session.nonEmpty ) {
			val attr = Option(session.get.getAttribute(key))

			if( attr.nonEmpty ) {
				session.get.removeAttribute(key)
				Option(attr.get.toString)
			}else{
				None
			}
		}else{
			None
		}
	}

//	def OrNotFound[T](o: Option[T])(f: T => Unit)(implicit request: HttpRequest): Unit = o match {
//		case Some(oo) => f(oo)
//		case _ => NotFound()
//	}

//	def getSessionValue(key: String): Option[String] = Option(session.get(key))
	case class PathKeyNotFoundException(key: String) extends Exception
	case class PathKeyFormatException(key: String, expected: String) extends Exception
	case class HeaderKeyNotFoundException(key: String) extends Exception
	case class HeaderFormatException(key: String, expected: String) extends Exception

	def getHeader(key: String): Option[String] = headers.get(key)

	private def testIfHeaderKeyValueIsEmpty[T](key: String, value: Option[T], required: Boolean): Option[T] = {
		if(required && value.isEmpty) throw PathKeyNotFoundException(key)
		else value
	}

	def headerKeyString[T](key: String, required: Boolean)(func: Option[String] => T): Unit =
		try func( testIfHeaderKeyValueIsEmpty(key,getPathKey(key),required ))
		catch { case _: NumberFormatException => throw PathKeyFormatException(key, "String") }


	def getPathKey(key: String): Option[String] = pathKeyValues.get(key)

	def PathKeyString[T](key: String)(func: String => T): T =
		try func(getPathKey(key).getOrElse(throw PathKeyNotFoundException(key)))
		catch { case _: NumberFormatException => throw PathKeyFormatException(key, "String") }

	def PathKeyInt[T](key: String)(func: Int => T): T =
		try func(getPathKey(key).map(_.toInt).getOrElse(throw PathKeyNotFoundException(key)))
		catch { case _: NumberFormatException => throw PathKeyFormatException(key, "Int") }

	def PathKeyFloat[T](key: String)(func: Float => T): T =
		try func(getPathKey(key).map(string => java.lang.Float.parseFloat(string)).getOrElse(throw PathKeyNotFoundException(key)))
		catch { case _: NumberFormatException => throw PathKeyFormatException(key, "Float") }

	def PathKeyBoolean[T](key: String)(func: Boolean => T): T =
		try func(getPathKey(key).map(_.toBoolean).getOrElse(throw PathKeyNotFoundException(key)))
		catch { case _: NumberFormatException => throw PathKeyFormatException(key, "Boolean") }


	def getQueryParameter(key: String): Option[String] = queryParameters.get(key)

	def id = getPathKey("id")

	def spaceName = getPathKey("space")

	def languageCode = getQueryParameter(_Language)

	lazy val secure: Boolean = servletRequest.isSecure
	lazy val host: String = servletRequest.asInstanceOf[Request].getHttpFields.get("Host")

	lazy val start: DateTime = DateTime.now
	lazy val Limit: Int = {
		val lim = queryParameters.getOrElse("_limit", "40")
		if( StringUtils.isAllDigits(lim) ) lim.toInt else 40
	}
	lazy val Skip: Int = {
		val sk = queryParameters.getOrElse("_skip", "0")
		if( StringUtils.isAllDigits(sk) ) sk.toInt else 0
	}

	var errorDetectedGzip = false

	def decompressGzipStream(jb: StringBuffer): Boolean = {
		try{
			if( isGzip ) {
				val gzipInputStream = new GZIPInputStream(servletRequest.getInputStream)
				val decoder = new InputStreamReader(gzipInputStream, "UTF-8")
				val br = new BufferedReader(decoder)
				var inputLine: Option[String] = Option(br.readLine)

				while (inputLine.isDefined) {
					if (jb.length() > 0)
						jb.append(System.getProperty("line.separator"))
					jb.append(inputLine.getOrElse(""))
					inputLine = Option(br.readLine)
				}

				gzipInputStream.close()

				true
			}else{
				false
			}
		}catch{
			case e: Throwable => {
				errorDetectedGzip = true
				logger.error(s"${servletRequest.getRequestURI}")
				logger.error(s"failed to read stream as zipped content ${e.getMessage}")
				false
			}
		}
	}

	def postBodyAsString: String = {
		val jb = new StringBuffer()

		if( !decompressGzipStream(jb) ) {
			var line: String = null
			try {
				val reader = servletRequest.getReader
				while({line = reader.readLine;  line!= null}){
					jb.append(line)
				}
			} catch { case e: Exception => }
		}

		jb.toString.replaceAll("[\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u000b\u000c\u000e\u000f\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001a\u001b\u001c\u001d\u001e\u001f]", "")
	}

	// cannot be lazy or a val as this is the primary method that will render postBody
//	def parsePostData: Either[Seq[AppError], FormData] = {
//		val isMultipartContent: Boolean = ServletFileUpload.isMultipartContent(servletRequest)
//		var formFieldData: util.HashMap[String, String] = new util.HashMap[String, String]()
//
//		if ( isMultipartContent ) {
//			val factory = new DiskFileItemFactory
//			val upload = new ServletFileUpload(factory)
//			val fields = upload.parseRequest(servletRequest)
//			val it = fields.iterator()
//
//
//			var fileData: Seq[(String, FormFile)] = Seq.empty
//
//
//			while (it.hasNext) {
//				val fileItem = it.next()
//				val isFormField = fileItem.isFormField
//
//				if(isFormField){
//					formFieldData.put(fileItem.getFieldName, fileItem.getString)
//				}else{
//					val file = File.createTempFile(DataStoreCore.getNextId, ".tmp")
//					fileItem.write(file)
//
//					fileData = fileData.:+( fileItem.getFieldName -> FormFile(
//						name = fileItem.getName,
//						contentType = fileItem.getContentType,
//						size = fileItem.getSize,
//						file = file
//					) )
//				}
//			}
//
//			Right( FormData(
//				formFields = formFieldData,
//				files = fileData
//			) )
//
//		}else{
//			postBody = parsePostStream
//
//			if(errorDetectedGzip){
//				logger.error("GZIP parsePostStream" + postBody)
//			}
//
//			if( isJson ){
//				Right(FormData())
//			}else {
//				try {
//					postBody.split("&").foreach { entry =>
//						val term = entry.split("=")
//
//						formFieldData.put(StringUtils.decode(term(0)), if (term.length == 2) StringUtils.decode(term(1)) else "")
//					}
//
//					Right(FormData(
//						formFields = formFieldData
//					))
//				}catch{
//					case e: Throwable => {
//						logger.error("parsePostData, trying to parse body as a simple/basic POST form" + e)
//						Left( Seq(ValidationFailed(message = "parsing body as a simple/basic POST form failed, submited object doesnt meet required criteria")) )
//					}
//				}
//			}
//		}
//	}
}
