package com.competitionlabs.http

case class StructuredData(
                           queryKeySet: Seq[String],
                           map: Map[String, String]
                         ){
  def getQueryString(key: String): Option[String] = map.get(key)
}

object SimpleDatastoreQueryWithRequest {

  val ID = "_id"
  val Skip = "_skip"
  val Limit = "_limit"
  val SortByFields = "_sortByFields"
  val AggregateBy = "_aggregateBy"
  val SearchFields = "_searchFields"
  val SearchFor = "_searchFor"
  val HasValuesFor = "_hasValuesFor"
  val HasNoValueFor = "_hasNoValuesFor"




  def must[A](searchClazz: Class[_], term: String, `value`: String, ignoreFields: Seq[String] = Seq(), skip: Int = 0, limit: Int = 20, forceSkipLimit: Boolean = false, includeFields: Seq[String] = Seq.empty, structuredData: Option[StructuredData] = None)(implicit request: HttpServletRequest, global: LabsGlobal): SimpleDatastoreQuery =
    create(searchClazz, Seq(DQFTermMust(term, `value`)), ignoreFields, skip, limit, includeFields = includeFields, forceSkipLimit = forceSkipLimit, structuredData = structuredData)

  def mustPost[A](searchClazz: Class[_], term: String, `value`: String, ignoreFields: Seq[String] = Seq(), skip: Int = 0, limit: Int = 20, forceSkipLimit: Boolean = false, includeFields: Seq[String] = Seq.empty)(implicit json: JValue, global: LabsGlobal): SimpleDatastoreQuery =
    createFromJsonPost(searchClazz, Seq(DQFTermMust(term, `value`)), ignoreFields, skip, limit, includeFields = includeFields, forceSkipLimit = forceSkipLimit)

  def should[A](searchClazz: Class[_], terms: Seq[DQFTermShould], ignoreFields: Seq[String] = Seq(), skip: Int = 0, limit: Int = 20, forceSkipLimit: Boolean = false, includeFields: Seq[String] = Seq.empty, structuredData: Option[StructuredData] = None)(implicit request: HttpServletRequest, global: LabsGlobal): SimpleDatastoreQuery =
    create(searchClazz, terms, ignoreFields, skip, limit, includeFields = includeFields, forceSkipLimit = forceSkipLimit, structuredData = structuredData)

  def create[A](searchClazz: Class[_], terms: Seq[DatastoreQueryFilterTerm] = Seq(), ignoreFields: Seq[String] = Seq(), skip: Int = 0, limit: Int = 20, forceSkipLimit: Boolean = false, includeFields: Seq[String] = Seq.empty, structuredData: Option[StructuredData] = None)(implicit request: HttpServletRequest, global: LabsGlobal): SimpleDatastoreQuery = {

    implicit val str: StructuredData = {
      if(structuredData.nonEmpty)
        structuredData.get
      else
        StructuredData(
          queryKeySet = request.getParameterNames.toSeq,
          map = request.getParameterMap.map(p => p._1 -> p._2.head).toMap
        )
    }


    createFromStructuredData[A](
      searchClazz = searchClazz,
      terms = terms,
      ignoreFields = ignoreFields,
      skip = skip,
      limit = limit,
      forceSkipLimit = forceSkipLimit,
      includeFields = includeFields
    )
  }

  def createFromJsonPost[A](searchClazz: Class[_], terms: Seq[DatastoreQueryFilterTerm] = Seq(), ignoreFields: Seq[String] = Seq(), skip: Int = 0, limit: Int = 20, forceSkipLimit: Boolean = false, includeFields: Seq[String] = Seq.empty)(implicit json: JValue, global: LabsGlobal): SimpleDatastoreQuery = {

    val values = json.values.asInstanceOf[Map[String, Any]]
    val keySet = values.keySet
    val mappedValues = values.map{
      item =>
        (item._1, item._2 match {
          case a: BigInt => a.toString
          case b: Boolean => b.toString
          case d: String => d
          case _ => ""
        })
    }

    implicit val structuredData: StructuredData = StructuredData(
      queryKeySet = keySet.toSeq,
      map = mappedValues
    )


    createFromStructuredData[A](
      searchClazz = searchClazz,
      terms = terms,
      ignoreFields = ignoreFields,
      skip = skip,
      limit = limit,
      forceSkipLimit = forceSkipLimit,
      includeFields = includeFields
    )
  }
  /**
    * Create a data store elastic query using parameters provided in the request and
    * also thos parameter injected from the requesting methods
    *
    * @param searchClazz    The object that the query is mapped against
    * @param terms          The forced terms to filter on
    * @param skip           The number of records to skip
    * @param limit          The number of records to limit the result to
    * @param forceSkipLimit Force the use of the provided skip limit values rather than attempt to retrieve these from the request
    * @tparam A The type of the request object
    * @return The simple query builder
    */
  private def createFromStructuredData[A](searchClazz: Class[_], terms: Seq[DatastoreQueryFilterTerm] = Seq(), ignoreFields: Seq[String] = Seq(), skip: Int = 0, limit: Int = 20, forceSkipLimit: Boolean = false, includeFields: Seq[String] = Seq.empty)(implicit structuredData: StructuredData, global: LabsGlobal): SimpleDatastoreQuery = {

    /** Filter out all fields that we need to FILTER on */
    val ks = structuredData.queryKeySet flatMap { k => if (ignoreFields.contains(k)) None else Option(k) }
    val termsToMatch = ks.filter(!_.startsWith("_")).map{ filterTerm =>

      val query = structuredData.getQueryString(filterTerm).getOrElse("")
      val must = query.startsWith("=")
      val queryString = if( must ){ query.substring(1) }else{ query }


      queryString.split(",").flatMap(v => //FIXME: multiple filters suck balls
        if(v.nonEmpty) { Option(
          if (filterTerm.endsWith(">"))
            DQFTermGreaterThanEqual(filterTerm.substring(0, filterTerm.length - 1), v, must)
          else if (filterTerm.endsWith("<"))
            DQFTermLessThanEqual(filterTerm.substring(0, filterTerm.length - 1), v, must)
          else if( must )
            DQFTermMust(
              if (filterTerm == "id") ID else filterTerm,
              v
            )
          else
            DQFTermShould(
              if (filterTerm == "id") ID else filterTerm,
              v
            )
        )
        } else
          None

      )

    }

    /** Filter out all fields that we need to SEARCH on */
    val search =
      if (structuredData.map.contains(SearchFields)) {
        Option(DatastoreQueryFieldSearch(
          structuredData.getQueryString(SearchFields).get.split(","),
          structuredData.getQueryString(SearchFor).getOrElse("")))
      }
      else
        None

    /** Filter out all fields that we need to AGGREGATE on */
    val aggregate = structuredData.getQueryString(AggregateBy) match {
      case Some(_aggregateBy) =>
        Option(_aggregateBy.split(","))
      case _ =>
        None
    }

    /** Filter out all fields that we want to SORT on */
    val sort = structuredData.getQueryString(SortByFields) match {
      case Some(_sortByFields) =>
        Option(
          _sortByFields.split(",").map { field =>
            val split = field.split(":")
            DatastoreQuerySortBy(split(0), if (split.length > 1) split(1).toLowerCase.equals("desc") else false)
          })
      case _ => None
    }

    /** Filter out all fields that has no field values */
    val existsFieldValues: Array[DatastoreQueryFilterTerm] = structuredData.getQueryString(HasValuesFor) match {
      case Some(_hasValuesFor) =>
        _hasValuesFor.split(",").map( t => DQFFilterExists(t) )
      case _ =>
        Array.empty
    }

    /** Filter out all fields that has no field values */
    val missingFieldValues: Array[DatastoreQueryFilterTerm] = structuredData.getQueryString(HasNoValueFor) match {
      case Some(_hasNoValuesFor) =>
        _hasNoValuesFor.split(",").map{ t =>
          var split = t.split(":")
          DQFFilterMissing(split.head, {
            if(split.size > 1 && split.last == "should")
              false
            else
              true
          })
        }
      case _ =>
        Array.empty
    }

    val combinedTerms: Seq[DatastoreQueryFilterTerm] =
      termsToMatch.flatten ++
        terms ++
        existsFieldValues ++
        missingFieldValues

    SimpleDatastoreQuery(
      searchClazz = searchClazz,
      terms = combinedTerms,
      skip = if (forceSkipLimit) skip else _skip(skip),
      limit = if (forceSkipLimit) limit else _limit(limit),
      sortByFields = sort.getOrElse(Array()),
      aggregations = aggregate.getOrElse(Array()),
      search = search,
      includeFields = includeFields,
      searchFuzziness = 5,
      searchPrefixLength = 3
    )
  }

  private def _limit(altValue: Int)(implicit structuredData: StructuredData) = com.labs.utils.Helpers.toInt(structuredData.getQueryString(Limit), altValue)

  private def _skip(altValue: Int)(implicit structuredData: StructuredData) = com.labs.utils.Helpers.toInt(structuredData.getQueryString(Skip), altValue)

  def mustWithSeq[A](searchClazz: Class[_], searchFields: Seq[DatastoreQueryFilterTerm], ignoreFields: Seq[String] = Seq(), skip: Int = 0, limit: Int = 20, forceSkipLimit: Boolean = false, includeFields: Seq[String] = Seq.empty)(implicit request: HttpServletRequest, global: LabsGlobal): SimpleDatastoreQuery =
    create(searchClazz, searchFields, ignoreFields, skip, limit, includeFields = includeFields, forceSkipLimit = forceSkipLimit)

  def simpleMust(searchClazz: Class[_], term: String, `value`: String, skip: Int = 0, limit: Int = 20, includeFields: Seq[String] = Seq.empty)(implicit global: LabsGlobal): SimpleDatastoreQuery =
    simpleCreate(searchClazz, Seq(DQFTermMust(term, `value`)), skip, limit, includeFields = includeFields)

  def simpleCreate[A](searchClazz: Class[_], terms: Seq[DatastoreQueryFilterTerm] = Seq(), skip: Int = 0, limit: Int = 20, includeFields: Seq[String] = Seq.empty, sortByFields: Array[DatastoreQuerySortBy] = Array.empty)(implicit global: LabsGlobal): SimpleDatastoreQuery = {
    SimpleDatastoreQuery(
      searchClazz = searchClazz,
      terms = terms,
      skip = skip,
      limit = limit,
      sortByFields = sortByFields,
      aggregations = Array(),
      search = None,
      includeFields = includeFields,
      searchFuzziness = 5,
      searchPrefixLength = 3
    )
  }

  def directCreate[A](searchClazz: Class[_], terms: Seq[DatastoreQueryFilterTerm] = Seq(), aggregate: Array[String] = Array(), ignoreFields: Seq[String] = Seq(), skip: Int = 0, limit: Int = 20, forceSkipLimit: Boolean = false, includeFields: Seq[String] = Seq.empty)(implicit global: LabsGlobal): SimpleDatastoreQuery = {
    SimpleDatastoreQuery(
      searchClazz = searchClazz,
      terms = terms,
      skip = skip,
      limit = limit,
      aggregations = aggregate,
      includeFields = includeFields,
      searchFuzziness = 5,
      searchPrefixLength = 3
    )
  }
}
