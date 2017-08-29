package codec

import cats.data.{ NonEmptyList ⇒ NEL }

sealed trait Reason

case class MissingFields(fields: NEL[String]) extends Reason
object MissingFields {
  def apply(field: String): MissingFields = new MissingFields(NEL.of(field))
  def of(field: String, fields: String*) = new MissingFields(NEL.of(field, fields:_*))
}

