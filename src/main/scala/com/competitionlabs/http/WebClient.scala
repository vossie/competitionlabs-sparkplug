package com.competitionlabs.http

import java.net.URI
import java.util.concurrent.TimeUnit

class WebClient() extends LabsLogger {

  def httpClient: HttpClient = HttpRequests.httpClient

  val DefaultHeaders: Map[NodeAddress, Seq[NodeAddress]] = Map("Content-Type" -> Seq(MimeTypes.Type.MULTIPART_BYTERANGES.toString))

  def send[T<:Any](nodeAddress: NodeAddress, headers:Headers, method:HttpMethod, body: Option[T], basicAuth: Option[BasicAuthCredentials] = None): ContentResponse = {
    assert(nodeAddress.nonEmpty)

    try {
      val r = HttpRequests.httpClient.newRequest(nodeAddress)
      r.timeout(TaskWithWait.TwentySeconds, TimeUnit.MILLISECONDS)
      r.method(method)
      headers.map(v => v._2.map( k => r.header(v._1, k.toString) ))

      HttpRequests.prepareAuth(r, URI.create(nodeAddress), basicAuth)

      body.foreach( b =>
        r.content(new BytesContentProvider(Serializer.serialise( b )))
      )

      r.send()
    }
    catch {
      case ex: Exception =>
        // TODO: Handle the error properly and resend
        logger.error(s"Failed to send ${method.toString} message to $nodeAddress", ex)
        throw ex
    }
  }
}
