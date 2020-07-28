package com.competitionlabs.http

import java.util

trait LabsServerConnector extends LabsLogger {

	val name: String
	val contextPath: String
	val port: Int

	val enableHttp2: Boolean
	val enableCors: Boolean
	val enableGzip: Boolean

	def onLoadServlets(webServer:WebServer, global:LabsGlobal, context:ServletContextHandler): Seq[_ <: LabsHttpServlet]

	private var labsHttpServlets: Seq[_ <: LabsHttpServlet] = Seq.empty

	def getServlets: Set[_ <: LabsHttpServlet] = labsHttpServlets.toSet

	def deploy(webServer: WebServer, global: LabsGlobal):  Seq[_ <: LabsHttpServlet] = {
		assert(!NetworkChecker.isPortInUse("localhost", port), s"Web server cannot be initialised as the $port port is already in use.")

		logger.info(s"Launching @$name web server on port $port")

		val connector: ServerConnector =
			if(enableHttp2) new ServerConnector(webServer.server, webServer.http1ConnectionFactory, webServer.http2ConnectionFactory)
			else new ServerConnector(webServer.server, webServer.http1ConnectionFactory)

		connector.setName(name)
		connector.setPort(port)
		connector.setIdleTimeout(1200)

		val context: ServletContextHandler = new ServletContextHandler(ServletContextHandler.GZIP)
		context.setContextPath(contextPath)

		// Filters
		setCors(context)
		setGZip(context)

		labsHttpServlets = onLoadServlets(webServer, global, context)

		for (servlet <- labsHttpServlets) {
			logger.debug(s"Registering servlet ${servlet.pageTitle} ($servlet), ${servlet.contextPath}")
			context.addServlet(new ServletHolder(servlet), servlet.contextPath)
		}

		context.setVirtualHosts(Array("@"+name))

		webServer.handlerCollection.addHandler(context)
		webServer.server.addConnector(connector)

		logger.info(s"@$name registered ${labsHttpServlets.size} servlets")

		labsHttpServlets
	}

	private def setCors(context: ServletContextHandler): Unit = if(enableCors){

		val cors = context.addFilter(classOf[CrossOriginFilter], "/*", util.EnumSet.of(DispatcherType.REQUEST))

		cors.setInitParameter(CrossOriginFilter.ALLOWED_ORIGINS_PARAM, "*")
		cors.setInitParameter(CrossOriginFilter.ALLOWED_HEADERS_PARAM, "X-Requested-With,Content-Type,Accept,Origin,X-Api-Key,Cache-Control")
		cors.setInitParameter(CrossOriginFilter.ALLOWED_METHODS_PARAM, s"${HttpMethod.GET.name()},${HttpMethod.POST.name()},${HttpMethod.PUT.name()},${HttpMethod.DELETE.name()},${HttpMethod.HEAD.name()},${HttpMethod.OPTIONS.name()}")
		cors.setInitParameter(CrossOriginFilter.ACCESS_CONTROL_ALLOW_ORIGIN_HEADER, "*")
		cors.setInitParameter(CrossOriginFilter.PREFLIGHT_MAX_AGE_PARAM, "5184000")
		cors.setInitParameter(CrossOriginFilter.CHAIN_PREFLIGHT_PARAM, "false")
	}

	private def setGZip(context: ServletContextHandler): Unit = if(enableGzip){

		val gzip = new GzipHandler
		gzip.setExcludedMimeTypes("text/event-stream")
		gzip.setIncludedMethods(HttpMethod.GET.name(), HttpMethod.POST.name(), HttpMethod.PUT.name())
		gzip.setMinGzipSize(254)
		context.setGzipHandler(gzip)
	}
}
