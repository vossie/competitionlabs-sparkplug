package com.labs.http.sparkplug

import scala.collection.mutable

class HttpHeaders(map: mutable.HashMap[String, String]){
	def get(key: String): Option[String] = {
		map.get(key.toLowerCase)
	}

	def getMap: mutable.HashMap[String, String] = map

	def add(key: String, value: String): Option[String] = {
		map.put(key, value)
	}
}
