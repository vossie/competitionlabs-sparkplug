package com.labs.http.sparkplug

import com.competitionlabs.http
import com.competitionlabs.http.{ServiceController, Status}
import com.labs.domain.models.{AccountId, Json}
import com.labs.errors.AppError
import com.labs.http.sparkplug.RoutingResponseHandler.Chunker
import com.labs.utils.StringUtils
import com.labs.utils.logging.LabsLogger
import com.labs.validators.ValidationException

trait SparkPlugResponses extends LabsLogger{

	final val TextCsvMime = "text/csv"

	def statusCodeConfirmation(statusCode: Int)(implicit request: HttpRequest): Int = {
		val errorCodeParameter = request.getQueryParameter("_el")
		if( errorCodeParameter.nonEmpty ){
			val errorCode = errorCodeParameter.getOrElse("")
			val parsedStatusCode = if( StringUtils.isAllDigits(errorCode) ){
				try{
					errorCode.toInt
				}catch{
					case e: Exception => Status.BAD_REQUEST
				}
			}else
				http.Status.BAD_REQUEST

			if( parsedStatusCode >= http.Status.OK && parsedStatusCode < http.Status.INTERNAL_SERVER_ERROR ){
				parsedStatusCode
			}else
				statusCode
		}else
			statusCode

	}

//	def globalErrorHandlingResponses(f: => Unit)(implicit request: HttpRequest): Unit = {
//		try f
//		catch {
//			case err: ValidationException =>
//				val error = ServiceController.badRequestAsJsObject(err.cause)
//				val firstError = err.cause.headOption
//				val errorStatus = if( firstError.nonEmpty ) firstError.get.status else 400
//				BadRequest(Json.fromJsonToString(error), RoutingResponseHandler.json, statusCodeConfirmation(errorStatus) )
//
//			case err: AppError =>
//				val error = ServiceController.badRequestAsJsObject(Seq(err))
//				BadRequest(Json.fromJsonToString(error), RoutingResponseHandler.json, statusCodeConfirmation(err.status) )
//
//			case err: Throwable => {
//				err.printStackTrace()
//				logger.error("Something went terribly wrong", err)
//
//				ServerNotReady()
//			}
//		}
//	}

	def Ok[T](body: T, f: T => Unit)(implicit request: HttpRequest): Unit = {
		request.servletResponse.setStatus( statusCodeConfirmation(200) )
		f(body)
	}

	def BadRequest[T](body: T, f: (T) => Unit, statusCode: Int)(implicit request: HttpRequest): Unit = {
		request.servletResponse.setStatus( statusCodeConfirmation(statusCode) )
		f(body)
	}

	def BadRequest(responseBody: String = "", statusCode: Int)(implicit request: HttpRequest): Unit = {
		request.servletResponse.setStatus( statusCodeConfirmation(statusCode) )
		request.servletResponse.setCharacterEncoding("UTF-8")
		request.servletResponse.setContentType("application/json")
		request.servletResponse.getWriter.write(s"""{"status":${statusCode}""")
	}

	def NotImplemented(responseBody: String = "")(implicit request: HttpRequest): Unit = {
		request.servletResponse.setStatus(501)
		request.servletResponse.setContentType("text/html")
		request.servletResponse.setCharacterEncoding("UTF-8")
		request.servletResponse.getWriter.write(responseBody)
	}

	def Unauthorized(responseBody: String = "")(implicit request: HttpRequest): Unit = {
		request.servletResponse.setStatus(401)
		request.servletResponse.setCharacterEncoding("UTF-8")
		request.servletResponse.setContentType("application/json")
		request.servletResponse.getWriter.write("""{"status":401}""")
	}

	def NotFound(responseBody: String = "")(implicit request: HttpRequest): Unit = {
		request.servletResponse.setStatus( statusCodeConfirmation(404) )
		request.servletResponse.setCharacterEncoding("UTF-8")
		request.servletResponse.setContentType("application/json")
		request.servletResponse.getWriter.write("""{"status":404}""")
	}

	def ServerNotReady(responseBody: String = "")(implicit request: HttpRequest): Unit = {
		request.servletResponse.setStatus(503)
		request.servletResponse.setCharacterEncoding("UTF-8")
		request.servletResponse.setContentType("application/json")
		request.servletResponse.getWriter.write("""{"status":503}""")
	}

	def SSERequest(channel: String, accountId: AccountId, routingId: Int)(implicit request: HttpRequest): Unit =
		request.sseRequest.map( sse => sse(channel, accountId, routingId)(request) ).getOrElse(NotFound())

	def ReturnRootList(statusCode: Int = 200, includeMeta: Boolean = true, contentType:String = "application/json", fileName:String="")(implicit request: HttpRequest): Chunker = {
		request.servletResponse.setStatus(statusCode)
		if(contentType==TextCsvMime){
			request.servletResponse.setHeader("content-disposition",s"""attachment; filename="$fileName"""")
		}
		RoutingResponseHandler.chunkedResponse(contentType)
	}
}

