package com.competitionlabs.http

trait LabsRequestParsers extends LabsHttpHelpers {

  /**
    * Verify that the declared parameters exist in the request object
    * @param params The parameter names to check for
    * @param f The function to execute if all the parameter exist
    * @param request The request
    * @param response The response
    */
  def VerifyParams(params: String*)(f: => Unit)(implicit request: HttpServletRequest, response: HttpServletResponse): Unit = {

    val reqParams = request.getParameterNames.asScala

    val result = params.flatMap(p =>
      if (reqParams.contains(p)) None
      else Option(p)
    )

    if(result.nonEmpty || reqParams.isEmpty)
      UnprocessibleEntityError(
        s"Unable to process the request as the following parameters are missing, ${result.mkString(", ")}."
      )
    else f
  }

  /**
    * this function assumes the parameter in the path is the account identifier
    * @param withAccount Function to execute if the path has a parameter after the context path
    * @param orElse Function to execute if the path does not have a parameter after the context path
    * @param request The request
    * @param response The response
    * @param global The mv global instance
    */
  def WithAccountIdOrElse(withAccount: AccountId => Unit)(orElse: => Unit)(implicit request: HttpServletRequest, response: HttpServletResponse, global: LabsGlobal): Unit =
    getPathParameters.headOption match {

      case Some(accountId: AccountId) =>
        //TODO: Add a check to ensure the ID is a valid account ID
        try
          withAccount(accountId)
        catch {
          case e:RecordNotFoundError =>
            NotFound(e)
        }

      case _ =>
        orElse
    }


  /**
    * this function assumes the parameter in the path is the identifier
    * @param withId Function to execute if the path has a parameter after the context path
    * @param orElse Function to execute if the path does not have a parameter after the context path
    * @param request The request
    * @param response The response
    * @param global The mv global instance
    */
  def WithPathIdOrElse(withId: String => Unit)(orElse: => Unit)(implicit request: HttpServletRequest, response: HttpServletResponse, global: LabsGlobal): Unit =
    getPathParameters.headOption match {

      case Some(id: String) =>
        try
          withId(id)
        catch {
          case e:RecordNotFoundError =>
            NotFound(e)
        }

      case _ =>
        orElse
    }

  /**
    * this function assumes the parameter in the path is the identifier
    * @param withParams Function to execute if the path has a parameter after the context path
    * @param orElse Function to execute if the path does not have a parameter after the context path
    * @param request The request
    * @param response The response
    * @param global The mv global instance
    */
  def WithPathParametersOrElse(withParams: Array[String] => Unit)(orElse: => Unit)(implicit request: HttpServletRequest, response: HttpServletResponse, global: LabsGlobal): Unit = {
    val params = getPathParameters
    if(params.nonEmpty) withParams(params)
    else orElse
  }

  /**
    * this function assumes the parameter in the path is the identifier
    * @param withQuery Function to execute if the path has a parameter after the context path
    * @param orElse Function to execute if the path does not have a parameter after the context path
    * @param request The request
    * @param response The response
    * @param global The mv global instance
    */
  def WithPathQueryOrElse(withQuery: String => Unit)(orElse: => Unit)(implicit request: HttpServletRequest, response: HttpServletResponse, global: LabsGlobal): Unit =
    getPathParameters.headOption match {

      case Some(pathParam: String) =>
        if(pathParam == "_query") {
          try
            withQuery(pathParam)
          catch {
            case e: RecordNotFoundError =>
              NotFound(e)
          }
        } else
          OperationNotAllowed(s"Invalid path parameter specified - $pathParam")
      case _ =>
        orElse
    }

  def OnContentType(onJson: => Unit, onBinary: => Unit, onOther: => Unit)(implicit request: HttpServletRequest, response: HttpServletResponse, global: LabsGlobal): Unit = {
    val ct = Option(request.getContentType).getOrElse(MimeTypes.Type.TEXT_HTML.toString).toLowerCase
    if(ct == MimeTypes.Type.APPLICATION_JSON.toString)
      onJson
    else if(ct == MimeTypes.Type.MULTIPART_BYTERANGES.toString)
      onBinary
    else
      onOther

  }
}
