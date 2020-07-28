package com.labs.http.sparkplug

import com.labs.errors.AppError
import com.labs.errors.CommonErrors.{ParameterNotInRangeError, RecordNotFound, ValidationFailed}
import com.labs.utils.logging.LabsLogger
import com.labs.validators.ValidationException
import org.apache.commons.csv.{CSVFormat, CSVRecord}
import org.json4s.{Formats, MappingException}

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe

trait SparkPlugMarshaller extends LabsLogger{

	final val MimeTypeJson = "application/json"
	final val MimeTypeHtml = "text/html"
	final val MimeTypeCsv = "text/csv"
	final val AcceptEncodingKey = "Accept-Encoding"
	final val UTF8 = "UTF-8"

	val json: SparkPlugJson

	val views: SparkPlugViews

	def onException(cause: Exception)(implicit request: HttpRequest): Exception = cause match {
		case e: MappingException =>
			ValidationFailed(e.msg.replace("\n",". ").replace("java.lang.",""))

		case e: java.util.NoSuchElementException =>
			ValidationFailed(e.getMessage)
			
		case _ =>
			cause
	}

	def errorDetail(errorCode: Int, description: String) = s"""{"errorCode": $errorCode,"description": "$description"}"""

	def errorResponseJson(statusCode: Int, errorCode: Int, description: String)(implicit request: HttpRequest): Unit = {

		request.servletResponse.setStatus(statusCode)
		request.servletResponse.setContentType(MimeTypeJson)
		request.servletResponse.setCharacterEncoding(UTF8)
		request.servletResponse.getWriter.write(s"""{"errors":{"errorCode": $errorCode,"message": "$description"}}""")
	}

	def errorResponseJsonWithDetail(statusCode: Int, errorCode: Int, description: String, detail:Seq[String])(implicit request: HttpRequest): Unit = {

		request.servletResponse.setStatus(statusCode)
		request.servletResponse.setContentType(MimeTypeJson)
		request.servletResponse.setCharacterEncoding(UTF8)

		def detailToJson(): String = {
			if(detail.nonEmpty){
				val x = detail.mkString(",")
				s""",{"detail":[$x]}"""
			}
			else ""
		}
		request.servletResponse.getWriter.write(s"""{"errors":{"errorCode": $errorCode,"message": "$description" ${detailToJson()}}}""")
	}

	def complete[T <: AnyRef](status: Int, mimeType: String = MimeTypeJson, format: Formats = json.json4sFormatsIgnoreNull)(body: T)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit= {
		request.servletResponse.setStatus(status)
		request.servletResponse.setContentType(mimeType)
		request.servletResponse.setCharacterEncoding(UTF8)

		request.servletResponse.getWriter.write(serialise[T](mimeType,body, format))
	}

	def complete200[T <: AnyRef](mimeType: String = MimeTypeJson, format: Formats = json.json4sFormatsIgnoreNull)(body: T)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit= {
		complete[T](200, mimeType, format)(body)
	}

	def complete200Default[T <: AnyRef]()(implicit request: HttpRequest): Unit= {
		request.servletResponse.setStatus(200)
		request.servletResponse.setContentType(MimeTypeJson)
		request.servletResponse.setCharacterEncoding(UTF8)
		request.servletResponse.getWriter.close()
	}

	def complete201[T <: AnyRef](errorCode: Int, message: String)(implicit request: HttpRequest): Unit= {
		errorResponseJson(201, errorCode, message)
	}
	def complete202[T <: AnyRef](errorCode: Int, message: String)(implicit request: HttpRequest): Unit= {
		errorResponseJson(202, errorCode, message)
	}
	def complete302(redirectUrl: String)(implicit request: HttpRequest): Unit= {
		request.servletResponse.setStatus(302)
		request.servletResponse.sendRedirect(redirectUrl)
	}
	def complete400[T <: AnyRef](errorCode: Int, message: String)(implicit request: HttpRequest): Unit= {
		errorResponseJson(400, errorCode, message)
	}
	def complete401[T <: AnyRef](errorCode: Int, message: String)(implicit request: HttpRequest): Unit= {
		errorResponseJson(401, errorCode, message)
	}
	def complete402[T <: AnyRef](errorCode: Int, message: String)(implicit request: HttpRequest): Unit= {
		errorResponseJson(402, errorCode, message)
	}
	def complete403[T <: AnyRef](errorCode: Int, message: String)(implicit request: HttpRequest): Unit= {
		errorResponseJson(403, errorCode, message)
	}
	def complete404[T <: AnyRef](errorCode: Int, message: String)(implicit request: HttpRequest): Unit= {
		errorResponseJson(404, 1007, message)
	}
	def complete405[T <: AnyRef](errorCode: Int, message: String)(implicit request: HttpRequest): Unit= {
		errorResponseJson(405, errorCode, message)
	}
	def complete413[T <: AnyRef](errorCode: Int, message: String)(implicit request: HttpRequest): Unit= {
		errorResponseJson(413, errorCode, message)
	}
	def complete500[T <: AnyRef](errorCode: Int, message: String)(implicit request: HttpRequest): Unit= {
		errorResponseJson(500, errorCode, message)
	}
	def complete501[T <: AnyRef](errorCode: Int, message: String)(implicit request: HttpRequest): Unit= {
		errorResponseJson(501, errorCode, message)
	}

	def Return[T <: AnyRef](mimeType: Seq[String] = Seq(MimeTypeJson))(func: => T)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit = {
		try complete[T](200, getResponseType(mimeType))(func)
		catch {
			case err: ValidationException =>
				errorResponseJsonWithDetail(getHttpStatus(err), err.code, err.message, err.cause.map(cause => errorDetail(cause.code,cause.message)))
			case err: AppError =>
				errorResponseJsonWithDetail(getHttpStatus(err), err.code, err.message, err.children.getOrElse(Seq.empty).map(cause => errorDetail(cause.code,cause.message)))
			case err: Throwable =>
				err.printStackTrace()
				logger.error("Something went terribly wrong", err)
				errorResponseJson(500, -1, "Well that's embarrassing, something went boom, if this error persists please contact support.")
		}
	}

	def getHttpStatus(error: AppError): Int =
		if(error.status == 400 || error.status == 401 || error.status == 402 || error.status == 403 || error.status == 404 ||
			error.status == 405 || error.status == 413 || error.status == 500 || error.status == 501 || error.status == 503) error.status
		else 400

	def getResponseType(mimeType: Seq[String])(implicit request: HttpRequest): String = {
		val h = request.getHeader(AcceptEncodingKey).getOrElse(mimeType.head)
		if(mimeType.contains(h)) h
		else mimeType.head
	}

	def Ok(func: => Unit)(implicit request: HttpRequest): Unit = {
		try {
			func
			complete200Default()
		}
		catch{
			case err: ValidationException =>
				errorResponseJsonWithDetail(err.status, err.code, err.message, err.cause.map(cause => errorDetail(cause.code,cause.message)))

			case err: AppError =>
				errorResponseJsonWithDetail(err.status, err.code, err.message, err.children.getOrElse(Seq.empty).map(cause => errorDetail(cause.code,cause.message)))

			case err: Throwable =>
				err.printStackTrace()
				logger.error("Something went terribly wrong", err)
				errorResponseJson(500, -1, "Well that's embarrassing, something went boom, if this error persists please contact support.")
		}
	}

	/**
	  * Language Specific Primitives
	  * "String",
	  * "boolean",
	  * "Boolean",
	  * "Double",
	  * "Int",
	  * "Long",
	  * "Float",
	  * "Object",
	  * "Any",
	  * "List",
	  * "Seq",
	  * "Map",
	  * "Array"
	  */


	private final val StringTag = classTag[String]
	private final val BooleanTag = classTag[Boolean]
	private final val DoubleTag = classTag[Double]
	private final val IntTag = classTag[Int]
	private final val LongTag = classTag[Long]
	private final val FloatTag = classTag[Float]
	private final val ObjectTag = classTag[Object]
	private final val AnyTag = classTag[Any]
	private final val ListTag = classTag[List[_]]
	private final val SeqTag = classTag[Seq[_]]
	private final val MapTag = classTag[Map[_,_]]
	private final val ArrayTag = classTag[Array[_]]

	implicit def valueOf[T](prop: String)(implicit classTag: ClassTag[T], typeTag: universe.TypeTag[T]) : T =
		classTag.runtimeClass match {
			case tag if tag == StringTag.runtimeClass =>
				prop.asInstanceOf[T]

			case tag if tag == BooleanTag.runtimeClass =>
				prop.toBoolean.asInstanceOf[T]

			case tag if tag == DoubleTag.runtimeClass =>
				prop.toDouble.asInstanceOf[T]

			case tag if tag == IntTag.runtimeClass =>
				prop.toInt.asInstanceOf[T]

			case tag if tag == LongTag.runtimeClass =>
				prop.toLong.asInstanceOf[T]

			case tag if tag == FloatTag.runtimeClass =>
				prop.toFloat.asInstanceOf[T]

			case tag if tag == ObjectTag.runtimeClass =>
				prop.asInstanceOf[T]

			case _       =>
				throw new UnsupportedOperationException(s"Unsupported type: $classTag")
		}

	def pathParam[T](key: String)(f: T => Unit)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit = f(
		request.getPathKey(key).map(v => valueOf[T](v)).getOrElse(throw new NotImplementedError(s"Method not implemented for pathParam[${classTag.runtimeClass}]($key)"))
	)

	def pathParamAsOption[T](key: String)(f: Option[T] => Unit)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit = f(
		request.getPathKey(key).map(v => valueOf[T](v))
	)

	def queryParam[T](key: String)(f: List[T] => Unit)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit = queryParamAsOption[T](key) { qp =>
		f(qp.getOrElse(throw ValidationException(message = s"Query parameter $key not found.", cause = Seq.empty)))
	}

	def queryParamAsOption[T](key: String)(f: Option[List[T]] => Unit)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit = f(
		request.queryParameters.get(key).map( param =>
			param.split(",").map(valueOf[T]).toList
		)
	)

	def headerParam[T](key: String)(f: T => Unit)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit = headerParamAsOption[T](key){ hp =>
		f(hp.getOrElse(throw ValidationException(message = s"Query parameter $key not found.", cause = Seq.empty)))
	}

	def headerParamAsOption[T](key: String)(f: Option[T] => Unit)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit = f(
		request.getHeader(key).map(v => valueOf[T](v))
	)

	def formParam[T](key: String)(f: T => Unit)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit = formParamAsOption[T](key){ fp =>
		f(fp.getOrElse(throw ValidationException(message = s"Form parameter $key not found.", cause = Seq.empty)))
	}

	def formParamAsOption[T](key: String)(f: Option[T] => Unit)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit = f(
		Option(request.servletRequest.getParameter(key))
	)

	def cookieParam[T](key: String)(f: T => Unit)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit = cookieParamAsOption[T](key){ cp =>
		f(cp.getOrElse(throw ValidationException(message = s"Cookie value for $key not found.", cause = Seq.empty)))
	}

	def cookieParamAsOption[T](key: String)(f: Option[T] => Unit)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit = f(
		request.cookies.get(key.toLowerCase).map(x => valueOf[T](x.getValue))
	)

	def deserialise[T](supportedContentTypes: Seq[String])(f: T => Unit)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit = {
		json.fromJsonAsOption[T](request.postBodyAsString) match {
			case Some(body) => f(body)
			case _ => throw ValidationException(message = "Unable to deserialize body.", cause = Seq.empty)
		}
	}

	def deserialiseSeq[T](supportedContentTypes: Seq[String])(f: Seq[T] => Unit)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): Unit = {
		json.fromJsonAsSeqOption[T](request.postBodyAsString) match {
			case Some(body) => f(body)
			case _ => throw ValidationException(message = "Unable to deserialize body.", cause = Seq.empty)
		}
	}

	def deserialiseCsv[T](onRecord: CSVRecord => T)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): List[T] = try {
		val bodyList = scala.collection.mutable.ListBuffer[T]()

		val csvParser = CSVFormat.RFC4180.withFirstRecordAsHeader().parse(request.servletRequest.getReader)
		val it = csvParser.iterator()

		while (it.hasNext){
			val record: CSVRecord = it.next()
			bodyList += onRecord(record)
		}

		bodyList.toList
	}
	catch { case e: IllegalArgumentException => throw ValidationException(cause = Seq(ParameterNotInRangeError(message = e.getMessage))) }

	def valueFromCsvRecord(key: String, record: CSVRecord): String =
		try record.get(key)
		catch { case _: IllegalArgumentException => throw ValidationException(message = s"Mapping not found for $key at row ${record.getRecordNumber + 1}", cause = Seq.empty) }

	def serialise[T <: AnyRef](mimeType: String, body: T, format: Formats)(implicit request: HttpRequest, classTag: ClassTag[T], typeTag: universe.TypeTag[T], mf: Manifest[T]): String = {
		if(mimeType == MimeTypeJson)
			json.toJsString(body, format)
		else
			body.toString
	}

	def validateContentType(validTypes: Seq[String])(implicit request: HttpRequest): Unit = {
		request.getContentType match {
			case Some(contentType) => if(!validTypes.contains(contentType.toLowerCase)) throw new NotImplementedError(s"")
			case _ => throw new NotImplementedError(s"")
		}
	}

	def withAccountId(f: String => Unit)(implicit request: HttpRequest):Unit = request.session.get("accountId") match {
		case Some(accountId) => f(accountId.toString)
		case _ => throw RecordNotFound(message = "Invalid space.")
	}

	def withUserId(f: String => Unit)(implicit request: HttpRequest):Unit = request.session.get("userId") match {
		case Some(userId) => f(userId.toString)
		case _ => throw RecordNotFound(message = "Invalid user.")
	}
}
