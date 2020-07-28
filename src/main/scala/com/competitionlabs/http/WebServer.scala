package com.competitionlabs.http

import java.util.concurrent.ForkJoinPool

import com.competitionlabs.http
import com.labs.http.sparkplug.SparkPlugServerScanner
import com.labs.http.system.SystemSeverConnector

import scala.language.implicitConversions

object WebServer extends LabsLogger {

	def apply(executor: ForkJoinPool, systemServerPort: Int)(implicit global: LabsGlobal): WebServer = {

		val maxThreads = 2048
		val minThreads = 16
		val idleTimeout = 1200 // 60,000

		val threadPool = new QueuedThreadPool(maxThreads, minThreads, idleTimeout)
		threadPool.setName("QTP-HTTP-Server-" + threadPool.hashCode())
		val server:Server = new Server(threadPool)
		val http1ConnectionFactory: HttpConnectionFactory = new HttpConnectionFactory()
		val http2ConnectionFactory: HTTP2CServerConnectionFactory= new HTTP2CServerConnectionFactory(new HttpConfiguration)

		http.WebServer(server, http1ConnectionFactory, http2ConnectionFactory, executor, SystemSeverConnector.apply(systemServerPort,  global.config.httpServerContext, global.config.httpPackageScan))
	}
}

/**
  * Jetty based web server wrapper.
  * Not much more to say about that.
//  * @param port The port to use
//  * @param contextPath The context path
//  * @param rootPackageToScan The root state to scan for servlets to load
  */
final case class WebServer(server: Server,
						   http1ConnectionFactory: HttpConnectionFactory,
						   http2ConnectionFactory: HTTP2CServerConnectionFactory,
						   executor: ForkJoinPool,
						   systemServer: LabsServerConnector)
						  (implicit val global: LabsGlobal) extends LabsModule with LabsLogger {

	override def processKey: String = ProcessManager.CommonModules.WebServer.module

	/**
	  * HTTP2 Clear Text Connection factory.
	  * Curl command to check http2 enabled: curl --http2 -I http://localhost:15676
	  */
	lazy val callbackPath: String =
		s"http://${NetworkChecker.thisIpAddress.getHostAddress}:${systemServer.port}/"

	override lazy val dependencies: Seq[ProcessorKey] = Seq.empty
	override lazy val observers: Seq[LabsObserver[_]] = Seq.empty

	final val handlerCollection = new HandlerCollection()

	def addConnector(labsServerConnector: LabsServerConnector): Seq[_ <: LabsHttpServlet] = labsServerConnector.deploy(webServer = this, global)

	def getControllers: Set[_ <: LabsHttpServlet] = systemServer.getServlets

	override def isRunning: Boolean = server.isStarted

	override def init(): LabsModule = this

	override def execute(): LabsModule = {
		assert(!server.isStarted, "The web server has already been started")

		addConnector(systemServer)
		SparkPlugServerScanner.init(server = this)
		server.setHandler(handlerCollection)
		server.start()

		logger.info(s"Server launched at $callbackPath")
		this
	}

	override def cancel(): LabsModule = {
		server.setStopTimeout(1000L)
		server.setStopAtShutdown(true)
		server.stop()
		this
	}

	implicit def boolToDouble(b: Boolean): Double = if(b) 1 else 0

	override def metrics: Map[String, Any] = Map(
		processKey+".port" -> systemServer.port,
		processKey+".contextPath" -> systemServer.contextPath,
		processKey+".uri" -> callbackPath,
		processKey+".state" -> server.getState,
		processKey+".connectors" -> server.getConnectors.map(_.getName).mkString(", "),
		processKey+".beans" -> server.getBeans.map(_.toString).mkString(", "),
		processKey+".threads" -> server.getThreadPool.getThreads,
		processKey+".threads.idle" -> server.getThreadPool.getIdleThreads,
		processKey+".threads.low" -> server.getThreadPool.isLowOnThreads,
		processKey+".threads.idle" -> server.getThreadPool.getIdleThreads
	) ++ connectors().flatten ++ connected().flatten

	def connectors(): Array[Map[String, String]] = server.getConnectors.map { connector =>

		Map(
			s"$processKey.connectors.${connector.getName}.byteBufferPool" -> connector.getByteBufferPool.toString,
			s"$processKey.connectors.${connector.getName}.scheduler" -> s"isStarted: ${connector.getScheduler.isStarted}, isFailed: ${connector.getScheduler.isFailed}, isRunning: ${connector.getScheduler.isRunning}, isStopped: ${connector.getScheduler.isStopped}, ",
			s"$processKey.connectors.${connector.getName}.protocols" -> connector.getProtocols.mkString(", "),
			s"$processKey.connectors.${connector.getName}.beans" -> connector.getBeans.map(_.toString).mkString(", ")
		)
	}

	def connected(): Array[Map[String, String]] = server.getConnectors.map { connector =>
		connector.getConnectedEndPoints.map{endPoint =>
			s"$processKey.x.connectors.${connector.getName}.connectedEndPoints.${endPoint.hashCode()}" -> s"${endPoint.getRemoteAddress}, isOpen: ${endPoint.isOpen} isOptimizedForDirectBuffers: ${endPoint.isOptimizedForDirectBuffers}, isOutputShutdown: ${endPoint.isOutputShutdown} > ${endPoint.toString.replace(",", ",  ")}"
		}.toMap
	}
}
