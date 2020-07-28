package com.competitionlabs.http

package object http {

	case class BulkResponse[T](success: Seq[T], failed: (T, Seq[Exception]))
}
