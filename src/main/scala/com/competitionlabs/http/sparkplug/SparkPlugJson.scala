package com.labs.http.sparkplug

import com.labs.errors.AppErrorException
import com.labs.errors.CommonErrors.{InvalidJsonFormatError, ParameterNotInRangeError, UnprocessibleEntityError, ValidationFailed}
import com.labs.utils.date.FormatDateTime
import com.labs.utils.logging.LabsLogger
import com.labs.validators.ValidationException

import scala.reflect.ClassTag
import scala.reflect.runtime.universe

class SparkPlugJson extends LabsLogger{

	import org.joda.time.{DateTime, DateTimeZone}
	import org.json4s.JsonAST.JValue
	import org.json4s.jackson.JsonMethods
	import org.json4s.JsonAST.JString
	import org.json4s.jackson.Serialization
	import org.json4s.{DefaultFormats, Formats, _}

	final val hints: ShortTypeHints = ShortTypeHints(List())


	/** Json serialize Format ignore None */
	val json4sDefaultFormatsIgnoreNull: Formats = DefaultFormats.skippingEmptyValues + JodaDateTimeSerializer


	/** Json serialize Format ignore None */
	implicit val json4sFormatsIgnoreNull: Formats = json4sDefaultFormatsIgnoreNull + hints
	//				new ModelEnumNameSerializer(StrategyType) +
	//				new ModelEnumNameSerializer(MQStates) + hints

	case object JodaDateTimeSerializer extends CustomSerializer[DateTime](ser = _ => ( {
		case JString(s) => FormatDateTime.parseDateTime(s).get.withZone(DateTimeZone.UTC)
		case JNull => null
	}, {
		case d: DateTime =>
			JString(d.toString(FormatDateTime.JodaDateTimeMillisFormatter))
	}))

	def toJsString(objectToWrite: AnyRef, format: Formats): String = try {
		Serialization.write(objectToWrite)(format)
	} catch {
		case e:Exception =>
			logger.error(e.getMessage)
			e.toString
	}

	def fromAnyToJsonString(objectToWrite: AnyRef, format: Formats): String = try {
		Serialization.write(objectToWrite)(format)
	} catch {
		case e:Exception =>
			logger.error(e.getMessage)
			e.toString
	}

	def toJsValue(objectToWrite: AnyRef, format: Formats = json4sFormatsIgnoreNull): JValue = try {
		Extraction.decompose(objectToWrite)(format)
	} catch {
		case e:Exception =>
			logger.error(e.getMessage)
			JsonMethods.parse(e.toString)
	}

	def fromJsonObjectAsOption[T](objectToWrite: JValue, format: Formats = json4sFormatsIgnoreNull)(implicit tt: universe.TypeTag[T], ct: ClassTag[T], mf: Manifest[T]): Option[T] = try {
		Option(Extraction.extract(objectToWrite)(format, manifest))
	} catch {
		case e:Exception =>
			logger.error(s"TypeTag: ${tt.toString()}, ClassTag: ${ct.toString()} $objectToWrite", e)
			None
	}

	def fromJsonAsOption[T](jsonString: String, format: Formats = json4sFormatsIgnoreNull)(implicit tt: universe.TypeTag[T], ct: ClassTag[T], mf: Manifest[T]): Option[T] = try {
		Option(Serialization.read[T](jsonString)(format, mf))
	} catch {
		case e: MappingException =>
			throw ValidationException(cause = Seq(ParameterNotInRangeError(message = e.msg.replace("\n",". ").replace("java.lang.",""))))
		case e: java.util.NoSuchElementException =>
			throw ValidationException(cause = Seq(ParameterNotInRangeError(message = e.getMessage)))
		case e:Exception =>
			logger.error(s"TypeTag: ${tt.toString()}, ClassTag: ${ct.toString()}, Manifest: ${mf.toString()} $jsonString", e)
			throw ValidationException(cause = Seq(UnprocessibleEntityError(message = "Unable to deserialize body.")))
	}

	def fromJsonAsSeqOption[T](jsonString: String, format: Formats = json4sFormatsIgnoreNull)(implicit tt: universe.TypeTag[Seq[T]], ct: ClassTag[Seq[T]], mf: Manifest[Seq[T]]): Option[Seq[T]] = try {
		Option(Serialization.read[Seq[T]](jsonString)(format, mf))
	} catch {
		case e:Exception =>
			logger.error(s"TypeTag: ${tt.toString()}, ClassTag: ${ct.toString()}, Manifest: ${mf.toString()} $jsonString", e)
			None
	}

	def fromJsonToString(obj: JValue): String = JsonMethods.compact(JsonMethods.render(obj))
	def fromJsonToString(obj: JObject): String = JsonMethods.compact(JsonMethods.render(obj))
	def fromJsonToString(obj: JArray): String = JsonMethods.compact(JsonMethods.render(obj))

	def parse(str: String): JValue = try{
		org.json4s.native.JsonMethods.parse(str)
	}catch{
		case jsonError: org.json4s.ParserUtil.ParseException => throw new AppErrorException(Seq(InvalidJsonFormatError(message = jsonError.getMessage.replaceAll("\\n", " "))))
		case e:Exception =>
			logger.debug(s"Couldn't parse JSON string: $str", e)
			throw e
	}


	def getFromJValue[T <: Any: Manifest](key: String)(implicit a: JValue): T = {
		a.\(key).extract[T]
	}

	def getFromJValue[T <: Any: Manifest](key: String, a: JValue): T = {
		a.\(key).extract[T]
	}

	def getFromJValue[T <: Any: Manifest](a: JValue): T = {
		a.extract[T]
	}

	def getFromJValueAsOption[T <: Any: Manifest](key: String)(implicit a: JValue): Option[T] = {
		val lookup = a.\(key)
		if(lookup.canEqual(JNothing)) None else Option( lookup.extract[T] )
	}

	def getFromJValueAsOption[T <: Any: Manifest](key: String, a: JValue): Option[T] = {
		val lookup = a.\(key)
		if(lookup.canEqual(JNothing)) None else Option( lookup.extract[T] )
	}

	def keyExists(key: String)(implicit a: JValue): Boolean = if(a.\(key).canEqual(JNothing)) false else true

	def ifKeysExists[T](json: JValue, keys: String*)(f: JValue => T): T = {
		keys.foreach(k=>if(!keyExists(k)(json)) throw new AppErrorException(Seq(InvalidJsonFormatError(message = s"Missing key $k. This has to be defined"))))
		f(json)
	}
}
