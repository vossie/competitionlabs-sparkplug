package com.competitionlabs.http

import java.net.URLEncoder

trait LabsHttpHelpers extends LabsLogger {

  /**
    * Get the simple state name.
    * This will return the string at the position after the last "."
    */
  final lazy val packageName: String = {
    val n = getClass.getPackage.getName
    n.substring(n.lastIndexOf(".")+1)
  }

  /**
    * Translates a string into {code application/x-www-form-urlencoded}
    * format using a specific encoding scheme. This method uses the
    * supplied encoding scheme to obtain the bytes for unsafe
    * characters.
    * <p>
    * <em><strong>Note:</strong> The <a href=
    * "http://www.w3.org/TR/html40/appendix/notes.html#non-ascii-chars">
    * World Wide Web Consortium Recommendation</a> states that
    * UTF-8 should be used. Not doing so may introduce
    * incompatibilities.</em>
    *
    * @param   s { @code String} to be translated.
    * @return the translated { @code String}.
    * exception UnsupportedEncodingException
    *            If the named encoding is not supported
    * @see URLDecoder#decode(java.lang.String, java.lang.String)
    * @since 1.4
    */
  def urlEncode(s:String): String = URLEncoder.encode(s, "UTF-8")

  /**
    * Returns the value of a request parameter as a <code>String</code>, or
    * <code>null</code> if the parameter does not exist. Request parameters are
    * extra information sent with the request. For HTTP servlets, parameters
    * are contained in the query string or posted form data.
    */
  def getUriParameter(name: String)(implicit request: HttpServletRequest): String = request.getParameter(name)

  /**
    * Get parameters specified in the path
    * @param request The request
    * @return The array of parameters found
    */
  def getPathParameters(implicit request: HttpServletRequest): Array[String] = Option(request.getPathInfo) match {
    case Some(param) => if(param == "/") Array.empty else param.stripPrefix("/").split("/")
    case _ => Array.empty
  }


  /**
    * Helper to get the request body as a sequence of strings.
    * @param request The request
    * @return Request body as a sequence of strings
    */
  def requestBody(implicit request: HttpServletRequest): Seq[String] =
    Stream.continually(request.getReader.readLine()).takeWhile(_ != null)

  /**
    * Respond with 200 OK and message body
    * @param bodyParser The body parser function
    * @param request THe request object
    * @param response THe response object
    * @tparam T The body type
    */
  def Ok[T](body: T, bodyParser: T => Unit)(implicit request: HttpServletRequest, response: HttpServletResponse): Unit = {
    response.setStatus(HttpStatus.OK_200)
    bodyParser(body)
  }

  def Chunked(f: HttpServletResponse => Unit)(implicit request: HttpServletRequest, response: HttpServletResponse): Unit = {
    response.setStatus(HttpStatus.OK_200)
    response.setContentType(MimeTypes.Type.MULTIPART_BYTERANGES.toString)
    f(response)
  }

  /**
    * Respond with 422 Unprocessable-Entity and message body
    * @param body The response body
    * @param request THe request object
    * @param response THe response object
    */
  def UnprocessableEntity(body: Exception)(implicit request: HttpServletRequest, response: HttpServletResponse): Unit = {
    response.setStatus(HttpStatus.UNPROCESSABLE_ENTITY_422)
    prepareErrorResponse(body)
  }


  /**
    * Respond with 404 Not-Found and message body
    * @param body The response body
    * @param request THe request object
    * @param response THe response object
    */
  def NotFound(body: Exception = RecordNotFound())(implicit request: HttpServletRequest, response: HttpServletResponse): Unit = {
    response.setStatus(HttpStatus.NOT_FOUND_404)
    prepareErrorResponse(body)
  }

  /**
    * Respond with 500 Internal-Server-Error and message body
    * @param body The response body
    * @param request THe request object
    * @param response THe response object
    */
  def InternalServerError(body: Exception)(implicit request: HttpServletRequest, response: HttpServletResponse): Unit = {
    response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR_500)
    prepareErrorResponse(body)
  }

  /**
    * Respond with 500 Internal-Server-Error and message body
    * @param body The response body
    * @param request THe request object
    * @param response THe response object
    */
  def MethodNotAllowedError(body: Exception)(implicit request: HttpServletRequest, response: HttpServletResponse): Unit = {
    response.setStatus(HttpStatus.METHOD_NOT_ALLOWED_405)
    prepareErrorResponse(body)
  }

  /**
    * Respond with 500 Internal-Server-Error and message body
    * @param body The response body
    * @param request THe request object
    * @param response THe response object
    */
  def FailedDependencyError(body: Exception)(implicit request: HttpServletRequest, response: HttpServletResponse): Unit = {
    response.setStatus(HttpStatus.FAILED_DEPENDENCY_424)
    prepareErrorResponse(body)
  }


  private def prepareErrorResponse(body: Exception)(implicit request: HttpServletRequest, response: HttpServletResponse): Unit = {
    if(request.getContentType == MimeTypes.Type.APPLICATION_JSON.toString)
      Parser.json(models.Json.toJsValue(body))
    else if(request.getContentType == MimeTypes.Type.MULTIPART_BYTERANGES.toString)
      Parser.binary(body)
    else
      Parser.html(body.toString)
  }

  /**
    * Convenience functions used to parse the response object
    */
  object Parser {

    /**
      * Return the response as html
      * @param body The body of the response
      * @param response The response object
      */
    def html(body: String)(implicit response: HttpServletResponse): Unit = {

      response.setContentType(MimeTypes.Type.TEXT_HTML.toString)
      response.setCharacterEncoding(Charsets.UTF_8.name())

      response.getWriter.write(body)
      response.getWriter.close()
    }

    /**
      * Return the response as json formatted object
      * @param body The body of the response
      * @param response The response object
      */
    def json(body: JValue)(implicit response: HttpServletResponse): Unit = {

      response.setContentType(MimeTypes.Type.APPLICATION_JSON.toString)
      response.setCharacterEncoding(Charsets.UTF_8.name())
      response.getWriter.write(compact(render(body)))
      response.getOutputStream.close()
    }

    /**
      * Return the response as json formatted object
      * @param body The body of the response
      * @param response The response object
      */
    def binary(body: Any)(implicit response: HttpServletResponse): Unit = {

      response.setContentType(MimeTypes.Type.MULTIPART_BYTERANGES.toString)
      response.getOutputStream.write(Serializer.serialise(body))
      response.getOutputStream.close()
    }

  }
}
