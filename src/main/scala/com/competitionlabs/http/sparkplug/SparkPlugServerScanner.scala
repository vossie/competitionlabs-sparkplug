package com.labs.http.sparkplug

import java.lang.reflect.Modifier

import com.competitionlabs.http.{LabsServerConnector, WebServer}
import com.labs.globals.LabsGlobal
import com.labs.http.WebServer
import com.competitionlabs.http.WebServer.logger
import org.reflections.Reflections

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import scala.language.implicitConversions

object SparkPlugServerScanner {

	def init(server: WebServer)(implicit global: LabsGlobal): Unit =
		if(global.config.sparkPlugEnabled)
			scanForLabsServerConnectors(global.config.sparkPlugPackageScan,server).foreach(server.addConnector)

	def scanForLabsServerConnectors(rootPackageToScan: Seq[String], server: WebServer)(implicit global: LabsGlobal): Seq[_ <: LabsServerConnector] = {

		logger.info(s"Scanning for labs server connectors in ${rootPackageToScan.mkString(", ")}")

		val result: List[_ <: LabsServerConnector] = doPackageScanLoop(0,List.empty,rootPackageToScan, server)
		
		logger.info(s"Scan found ${result.size} labs server connector ${if(result.size>1) "s" else ""} in ${rootPackageToScan.mkString(", ")}")

		result.groupBy(_.port).map(serverConnectors => {

			if(serverConnectors._2.length>1)
				throw new RuntimeException(s"""Duplicate ports found for "${serverConnectors._1}" in ${serverConnectors._2.mkString(", ")} """)

			serverConnectors._2.head

		}).toSeq
	}

	@scala.annotation.tailrec
	private def doPackageScanLoop(count:Int, serverConnectors: List[_ <: LabsServerConnector], rootPackageToScan: Seq[String], server: WebServer)(implicit global: LabsGlobal): List[_ <: LabsServerConnector] = {

		logger.info(s"Preparing labs server connector for ${global.cluster.nodeAddress}")

		if(count < rootPackageToScan.length) {

			val reflections = new Reflections(rootPackageToScan(count))
			val controllers = reflections.getSubTypesOf(classOf[LabsServerConnector]).asScala.toList
			val filteredControllers = controllers.filter(cs => !cs.isInterface && !Modifier.isAbstract(cs.getModifiers))

			val out = filteredControllers.map { c =>
				logger.info(s"${c.toString}")
				val constructors = c.getConstructors
				val params = constructors.flatMap(_.getGenericParameterTypes)

				logger.info(s"Found ${c.getSimpleName} (${c.toString}) with ${params.length} parameters")

				if (params.length == 1 && params(0) == classOf[LabsGlobal])
					c.getConstructor(classOf[LabsGlobal]).newInstance(global)
				else if (params.length == 1 && params(0) == classOf[WebServer])
					c.getConstructor(classOf[WebServer]).newInstance(server)
				else if (params.length == 2 && params(0) == classOf[WebServer] && params(1) == classOf[LabsGlobal])
					c.getConstructor(classOf[WebServer], classOf[LabsGlobal]).newInstance(server,global)
				else
					c.getDeclaredConstructor().newInstance()
			}

			doPackageScanLoop(
				count+1,
				serverConnectors ++ out,
				rootPackageToScan,
				server
			)
		}
		else
			serverConnectors
	}
}






