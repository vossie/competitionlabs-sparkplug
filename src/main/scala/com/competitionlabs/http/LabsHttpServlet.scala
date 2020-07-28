package com.competitionlabs.http

trait LabsHttpServlet extends HttpServlet with LabsRequestParsers  {

  def pageTitle: String

  def contextPath: String

  def description: String

  final override def doHead(req: HttpServletRequest, resp: HttpServletResponse): Unit = Head(req, resp)
  final override def doGet(req: HttpServletRequest, resp: HttpServletResponse): Unit = Get(req,resp)
  final override def doPut(req: HttpServletRequest, resp: HttpServletResponse): Unit = Put(req, resp)
  final override def doPost(req: HttpServletRequest, resp: HttpServletResponse): Unit = Post(req, resp)
  final override def doDelete(req: HttpServletRequest, resp: HttpServletResponse): Unit = Delete(req, resp)

  /**
    * <p>Receives an HTTP HEAD request from the protected
    * <code>service</code> method and handles the
    * request.
    * The client sends a HEAD request when it wants
    * to see only the headers of a response, such as
    * Content-Type or Content-Length. The HTTP HEAD
    * method counts the output bytes in the response
    * to set the Content-Length header accurately.
    *
    * <p>If you override this method, you can avoid computing
    * the response body and just set the response headers
    * directly to improve performance. Make sure that the
    * <code>doHead</code> method you write is both safe
    * and idempotent (that is, protects itself from being
    * called multiple times for one HTTP HEAD request).
    *
    * <p>If the HTTP HEAD request is incorrectly formatted,
    * <code>doHead</code> returns an HTTP "Bad Request"
    * message.
    *
    * @param req  the request object that is passed to the servlet
    * @param resp the response object that the servlet
    *             uses to return the headers to the client
    * exception IOException   if an input or output error occurs
    * exception ServletException  if the request for the HEAD
    *            could not be handled
    */
  def Head(implicit req: HttpServletRequest, resp: HttpServletResponse): Unit = super.doHead(req, resp)

  /**
    * Called by the server (via the <code>service</code> method) to
    * allow a servlet to handle a GET request.
    *
    * <p>Overriding this method to support a GET request also
    * automatically supports an HTTP HEAD request. A HEAD
    * request is a GET request that returns no body in the
    * response, only the request header fields.
    *
    * <p>When overriding this method, read the request data,
    * write the response headers, get the response's writer or
    * output stream object, and finally, write the response data.
    * It's best to include content type and encoding. When using
    * a <code>PrintWriter</code> object to return the response,
    * set the content type before accessing the
    * <code>PrintWriter</code> object.
    *
    * <p>The servlet container must write the headers before
    * committing the response, because in HTTP the headers must be sent
    * before the response body.
    *
    * <p>Where possible, set the Content-Length header (with the
    * {javax.servlet.ServletResponse#setContentLength} method),
    * to allow the servlet container to use a persistent connection
    * to return its response to the client, improving performance.
    * The content length is automatically set if the entire response fits
    * inside the response buffer.
    *
    * <p>When using HTTP 1.1 chunked encoding (which means that the response
    * has a Transfer-Encoding header), do not set the Content-Length header.
    *
    * <p>The GET method should be safe, that is, without
    * any side effects for which users are held responsible.
    * For example, most form queries have no side effects.
    * If a client request is intended to change stored data,
    * the request should use some other HTTP method.
    *
    * <p>The GET method should also be idempotent, meaning
    * that it can be safely repeated. Sometimes making a
    * method safe also makes it idempotent. For example,
    * repeating queries is both safe and idempotent, but
    * buying a product online or modifying data is neither
    * safe nor idempotent.
    *
    * <p>If the request is incorrectly formatted, <code>doGet</code>
    * returns an HTTP "Bad Request" message.
    *
    * @param req an { @link HttpServletRequest} object that
    *                       contains the request the client has made
    *                       of the servlet
    * @param resp an { @link HttpServletResponse} object that
    *                        contains the response the servlet sends
    *                        to the client
    * exception IOException   if an input or output error is
    *            detected when the servlet handles
    *            the GET request
    * exception ServletException  if the request for the GET
    *            could not be handled
    * @see javax.servlet.ServletResponse#setContentType
    */
  def Get(implicit req: HttpServletRequest, resp: HttpServletResponse): Unit = super.doGet(req, resp)

  /**
    * Called by the server (via the <code>service</code> method)
    * to allow a servlet to handle a PUT request.
    *
    * The PUT operation allows a client to
    * place a file on the server and is similar to
    * sending a file by FTP.
    *
    * <p>When overriding this method, leave intact
    * any content headers sent with the request (including
    * Content-Length, Content-Type, Content-Transfer-Encoding,
    * Content-Encoding, Content-Base, Content-Language, Content-Location,
    * Content-MD5, and Content-Range). If your method cannot
    * handle a content header, it must issue an error message
    * (HTTP 501 - Not Implemented) and discard the request.
    * For more information on HTTP 1.1, see RFC 2616
    * <a href="http://www.ietf.org/rfc/rfc2616.txt"></a>.
    *
    * <p>This method does not need to be either safe or idempotent.
    * Operations that <code>doPut</code> performs can have side
    * effects for which the user can be held accountable. When using
    * this method, it may be useful to save a copy of the
    * affected URL in temporary storage.
    *
    * <p>If the HTTP PUT request is incorrectly formatted,
    * <code>doPut</code> returns an HTTP "Bad Request" message.
    *
    * @param req the { @link HttpServletRequest} object that
    *                        contains the request the client made of
    *                        the servlet
    * @param resp the { @link HttpServletResponse} object that
    *                         contains the response the servlet returns
    *                         to the client
    * exception IOException   if an input or output error occurs
    *            while the servlet is handling the
    *            PUT request
    * exception ServletException  if the request for the PUT
    *            cannot be handled
    */
  def Put(implicit req: HttpServletRequest, resp: HttpServletResponse): Unit = super.doPut(req, resp)

  /**
    * Called by the server (via the <code>service</code> method)
    * to allow a servlet to handle a POST request.
    *
    * The HTTP POST method allows the client to sendWebhookTransformerTrigger
    * data of unlimited length to the Web server a single time
    * and is useful when posting information such as
    * credit card numbers.
    *
    * <p>When overriding this method, read the request data,
    * write the response headers, get the response's writer or output
    * stream object, and finally, write the response data. It's best
    * to include content type and encoding. When using a
    * <code>PrintWriter</code> object to return the response, set the
    * content type before accessing the <code>PrintWriter</code> object.
    *
    * <p>The servlet container must write the headers before committing the
    * response, because in HTTP the headers must be sent before the
    * response body.
    *
    * <p>Where possible, set the Content-Length header (with the
    * {javax.servlet.ServletResponse#setContentLength} method),
    * to allow the servlet container to use a persistent connection
    * to return its response to the client, improving performance.
    * The content length is automatically set if the entire response fits
    * inside the response buffer.
    *
    * <p>When using HTTP 1.1 chunked encoding (which means that the response
    * has a Transfer-Encoding header), do not set the Content-Length header.
    *
    * <p>This method does not need to be either safe or idempotent.
    * Operations requested through POST can have side effects for
    * which the user can be held accountable, for example,
    * updating stored data or buying items online.
    *
    * <p>If the HTTP POST request is incorrectly formatted,
    * <code>doPost</code> returns an HTTP "Bad Request" message.
    *
    * @param req an { @link HttpServletRequest} object that
    *                       contains the request the client has made
    *                       of the servlet
    * @param resp an { @link HttpServletResponse} object that
    *                        contains the response the servlet sends
    *                        to the client
    * exception IOException   if an input or output error is
    *            detected when the servlet handles
    *            the request
    * exception ServletException  if the request for the POST
    *            could not be handled
    * @see javax.servlet.ServletOutputStream
    * @see javax.servlet.ServletResponse#setContentType
    */
  def Post(implicit req: HttpServletRequest, resp: HttpServletResponse): Unit = super.doPost(req, resp)

  /**
    * Called by the server (via the <code>service</code> method)
    * to allow a servlet to handle a DELETE request.
    *
    * The DELETE operation allows a client to remove a document
    * or Web page from the server.
    *
    * <p>This method does not need to be either safe
    * or idempotent. Operations requested through
    * DELETE can have side effects for which users
    * can be held accountable. When using
    * this method, it may be useful to save a copy of the
    * affected URL in temporary storage.
    *
    * <p>If the HTTP DELETE request is incorrectly formatted,
    * <code>doDelete</code> returns an HTTP "Bad Request"
    * message.
    *
    * @param req the { @link HttpServletRequest} object that
    *                        contains the request the client made of
    *                        the servlet
    * @param resp the { @link HttpServletResponse} object that
    *                         contains the response the servlet returns
    *                         to the client
    * exception IOException   if an input or output error occurs
    *            while the servlet is handling the
    *            DELETE request
    * exception ServletException  if the request for the
    *            DELETE cannot be handled
    */
  def Delete(implicit req: HttpServletRequest, resp: HttpServletResponse): Unit = super.doDelete(req, resp)
}
