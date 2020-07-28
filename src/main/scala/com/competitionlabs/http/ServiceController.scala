package com.competitionlabs.http

object ServiceController {

	type StatusId = Int

	def toJsonWithBrains[T](a:T): JValue = a match {
		case seq: Seq[_] => seq.map( model => model match {
			case aa: DataModel[_] => aa.toJson
			case aa: JObject      => aa
			case aa: AnyRef => toJsonWithBrains(aa)
		})
		case value: DataModel[_]  => value.toJson
		case _ => Json.toJsValue(a.asInstanceOf[AnyRef])
	}

	def ReturnFromEither[A](data: Either[Seq[AppError], A])(implicit global: LabsGlobal): Either[JObject, JObject]  = {
		data fold (
			errors => {
				Left(badRequestAsJsObject(errors))
			},
			ok => {
				val out = JObject(List(
					"meta" -> JObject( List("time" -> JString(new DateTime(DateTimeZone.UTC).toString))),
					"data" -> toJsonWithBrains(ok)
				))
				Right(out)
			})
	}

	def ReturnFromSeq[A <: DataModel[A]](data: Seq[A])(implicit global: LabsGlobal): Either[JObject, JObject] = ReturnFromEither(Right(data))

	def badRequestAsJsObject(errors: Seq[AppError]): JObject = {
		val jArray = JArray(errors.flatMap {
			case error: AppError => Option(error.toJson)
			case _ => None
		}.toList)
		"errors" -> jArray
	}

	def ReturnSeq[A](data: Seq[Either[Seq[AppError], A]])(implicit global: LabsGlobal): JObject  = {

		val errorObject = JObject(
			List("errors" -> JArray(
				data.flatMap{
					case Left(error) => Option(error)
					case _ => None
				}.flatMap{
					case error: AppError => Option(error.toJson)
					case _ => None
				}.toList
			))
		)

		errorObject ~ ("data" -> {
			val d = data.flatMap{
				case Right(res) => Option(res)
				case _ => None
			}
			toJsonWithBrains(d)
		})
	}

	def ReturnSeqErrors(errors: Seq[Seq[AppError]])(implicit global: LabsGlobal): Either[JArray, JArray]  = {
		if(errors.nonEmpty) {
			val err =   errors map ( e => JArray( e.map(e1=> e1.toJson).toList ))
			val errorObject = JArray( List( ("errors" -> toJsonWithBrains(err)).asInstanceOf[JObject] ))
			Left(errorObject)
		}
		else
			Right(JArray( List( (
				"meta" -> JObject( List("time" -> JString(new DateTime(DateTimeZone.UTC).toString))),
				"data" -> toJsonWithBrains("Ok")
			).asInstanceOf[JObject] )))
	}

	def ReturnFromOptionOfSeq(data: Option[Seq[AppError]]): Either[JObject, JObject]  = {
		data match {
			case Some(errors) =>
				Left(badRequestAsJsObject(errors))
			case _ =>
				Right(parse("""{}""").asInstanceOf[JObject])
		}
	}

//	def ReturnValidData[A <: DataModel[A]](data: A): Either[Seq[AppError], A] = data.isValid()

	// Delayed Return
	def DelayedReturn[A](milli:Int)(data: Either[Seq[AppError], A])(implicit global: LabsGlobal): Either[JObject, JObject] ={
		Thread.sleep(milli)
		ReturnFromEither(data)
	}

	def makeMetaFrom[A, I](data:QueryResult[A, I])(implicit global: LabsGlobal): json4s.JValue = JObject( List(
		"time" -> JString(new DateTime(DateTimeZone.UTC).toString),
		"totalRecordsFound" -> toJsonWithBrains(data.totalRecordsFound),
		"skip" -> toJsonWithBrains(data.skip),
		"limit" -> toJsonWithBrains(data.limit) ))


	def makeMetaForSingleRecord[A, I](data:QueryResult[A, I])(implicit global: LabsGlobal) = JObject( List(
		"time" -> JString(new DateTime(DateTimeZone.UTC).toString),
		"totalRecordsFound" -> toJsonWithBrains(1),
		"skip" -> toJsonWithBrains(data.skip),
		"limit" -> toJsonWithBrains(1) ))

	def ReturnQueryResult[A, I](data: Either[Seq[AppError], QueryResult[A, I]])(implicit global: LabsGlobal): Either[(StatusId, JObject), JObject]  = {
		data fold (
			errors => {
				Left( (errors.head.status, badRequestAsJsObject(errors)) )
			},
			queryResult => Right(ReturnPreparedQueryResult[A, I](queryResult)))
	}

	def ReturnPreparedQueryResult[A, I](queryResult: QueryResult[A, I])(implicit global: LabsGlobal): JObject  = {
		if (queryResult.aggregations.isEmpty)
			JObject(List(
				"meta" -> makeMetaFrom(queryResult),
				"data" -> toJsonWithBrains(queryResult.result)))
		else
			JObject(List(
				"meta" -> makeMetaFrom(queryResult),
				"aggregations" -> toJsonWithBrains(queryResult.aggregations),
				"data" -> toJsonWithBrains(queryResult.result)))
	}
}
