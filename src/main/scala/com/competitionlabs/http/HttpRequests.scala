package com.competitionlabs.http

import java.io.ByteArrayOutputStream
import java.net.URI
import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap

object HttpRequests {

	val idleTimeout: Int = 1000 * 30
	val maxThreads = 200
	val minThreads = 4

	val threadPool = new QueuedThreadPool(maxThreads, minThreads, idleTimeout)
	threadPool.setName("QTP-HTTP-Client-" + threadPool.hashCode())

	val ContentTypeKey: String = "Content-Type"
	val ApplicationJsonValue = "application/json"
	val AcceptEncodingKey = "Accept-Encoding"
	val ContentEncodingKey = "Content-Encoding"
	val GzipValue = "gzip"
	val connections = new ConcurrentHashMap[String, Request]()
	val header: Map[String, Seq[String]] = Map("Accept" -> Seq("application/json"))
	val headerWithBody: Map[String, Seq[String]] = Map("Accept" -> Seq(ApplicationJsonValue), ContentTypeKey -> Seq(ApplicationJsonValue))
	val CLRealm = "cl-realm"
	val AuthorizationKey = "Authorization"

	// Jetty client config
	val sslContextFactory = new SslContextFactory()
	val httpClient = new HttpClient(sslContextFactory)
	val auth: AuthenticationStore = httpClient.getAuthenticationStore

	httpClient.setTCPNoDelay(true)
	httpClient.setIdleTimeout(idleTimeout)
	httpClient.setRequestBufferSize(16 * 1024)
	httpClient.setFollowRedirects(true)
	httpClient.setUserAgentField(new HttpField(HttpHeader.USER_AGENT, "CompetitionLabs/1.0"))
	httpClient.setExecutor(threadPool)
	httpClient.start()

	def getContentType(headers: Map[String, Seq[String]]): String = headers(ContentTypeKey).headOption.getOrElse(ApplicationJsonValue)

	def prepareRequest(url:String, method: HttpMethod, headers: Map[String, Seq[String]], basicAuthCredentials: Option[BasicAuthCredentials]): Request = {

		val uri = new URI(url)
		val request: Request = httpClient.newRequest(url).method(method)

		/** Do not add content type key to the headers list if POST or PUT operation, it is done while preparing request **/
		if(method == HttpMethod.POST || method == HttpMethod.PUT)
			headers.foreach(h =>
				if(!(h._1 == ContentTypeKey)) {
					h._2.foreach(v =>
						request.header(h._1, v))
					}
			)

		else
			headers.foreach(h =>
				h._2.foreach(v =>
					request.header(h._1, v))
			)

		prepareAuth(request, uri, basicAuthCredentials)

		request
	}

	def prepareAuth(request: Request, uri: URI, basicAuthCredentials: Option[BasicAuthCredentials]): Unit = basicAuthCredentials.foreach { credentials =>
		if(auth.findAuthentication("basic", uri, CLRealm) == null) auth.addAuthentication(new BasicAuthentication(uri, CLRealm , credentials.username, credentials.password))

		val encodedCredentials: String = "Basic " + B64Code.encode(credentials.username + ":" + credentials.password, StandardCharsets.ISO_8859_1)
		request.header(AuthorizationKey, encodedCredentials)
	}

	// GET

	def httpGetRequest(url: String, headers: Map[String, Seq[String]] = header, basicAuthCredentials: Option[BasicAuthCredentials] = None): Either[Seq[AppError], HttpGetResponse] = {

		val request = prepareRequest(url, HttpMethod.GET, headers, basicAuthCredentials)
		val response = request.send()

		Right(HttpGetResponse(response.getContentAsString, response.getStatus))
	}

	def httpGetRequestAsync(url: String, headers: Map[String, Seq[String]] = header, basicAuthCredentials: Option[BasicAuthCredentials] = None)(onComplete: Response.CompleteListener): Either[Seq[AppError], HttpGetResponse] = {

		val request = prepareRequest(url, HttpMethod.GET, headers, basicAuthCredentials)
		val response = request.send()

		Right(HttpGetResponse(response.getContentAsString, response.getStatus))
	}

	// POST
	def httpPostRequest(url: String, postString: String, headers: Map[String, Seq[String]] = headerWithBody, basicAuthCredentials: Option[BasicAuthCredentials] = None, sendCompressed: Boolean = true): Either[Seq[AppError], HttpGetResponse] = {

		val response =
			if(sendCompressed) {
				val newHeaders: Map[String, Seq[String]] = if(headers.contains(ContentEncodingKey)) headers else headers + (ContentEncodingKey -> Seq(GzipValue))
				val request = prepareRequest(url, HttpMethod.POST, newHeaders, basicAuthCredentials)
				request.content(new BytesContentProvider(compress(postString)), getContentType(newHeaders)).send()
			}
			else {
				val request = prepareRequest(url, HttpMethod.POST, headers, basicAuthCredentials)
				val r = request.content(new StringContentProvider(getContentType(headers), postString, StandardCharsets.UTF_8), getContentType(headers))
				r.getHeaders.remove(AcceptEncodingKey)
				r.getHeaders.remove(ContentEncodingKey)
				r.send()
			}

		Right(HttpGetResponse(response.getContentAsString, response.getStatus))
	}

	// POST
	def httpPostRequestAsBytes(url: String, postBody: Array[Byte], headers: Map[String, Seq[String]] = headerWithBody, basicAuthCredentials: Option[BasicAuthCredentials] = None): Either[Seq[AppError], HttpGetResponseAsBytes] = {

		val newHeaders: Map[String, Seq[String]] = if(headers.contains(ContentEncodingKey)) headers else headers + (ContentEncodingKey -> Seq(GzipValue))
		val request = prepareRequest(url, HttpMethod.POST, newHeaders, basicAuthCredentials)
		val response = request.content(new BytesContentProvider(postBody), getContentType(newHeaders)).send()

		Right(HttpGetResponseAsBytes(response.getContent, response.getStatus))
	}

	// PUT

	def httpPutRequest(url: String, postString: String, headers: Map[String, Seq[String]] = headerWithBody, basicAuthCredentials: Option[BasicAuthCredentials] = None): Either[Seq[AppError], HttpGetResponse] = {

		val request = prepareRequest(url, HttpMethod.PUT, headers, basicAuthCredentials)
		val response = {
			val r = request.content(new StringContentProvider(getContentType(headers), postString, StandardCharsets.UTF_8), getContentType(headers))
			r.getHeaders.remove(AcceptEncodingKey)
			r.send()
		}

		Right(HttpGetResponse(response.getContentAsString, response.getStatus))
	}

	// DELETE

	def httpDeleteRequest(url: String, headers: Map[String, Seq[String]] = header, basicAuthCredentials: Option[BasicAuthCredentials] = None): Either[Seq[AppError], HttpGetResponse] = {

		val request = prepareRequest(url, HttpMethod.DELETE, headers, basicAuthCredentials)
		val response = request.send()

		Right(HttpGetResponse(response.getContentAsString, response.getStatus))
	}

	private def compress(data: String): Array[Byte] = {
		import java.util.zip.GZIPOutputStream
		val bos: ByteArrayOutputStream = new ByteArrayOutputStream(data.length)
		val gzip: GZIPOutputStream = new GZIPOutputStream(bos)
		gzip.write(data.getBytes)
		gzip.close()
		val compressed: Array[Byte] = bos.toByteArray
		bos.close()

		compressed

	}
}
