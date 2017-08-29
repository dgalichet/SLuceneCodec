import cats.Semigroup
import cats.data.Validated

package object codec {

  type DecodedResult[V] = Validated[MissingFields, V]

  implicit val missingFieldsErrorSG: Semigroup[MissingFields] = new Semigroup[MissingFields] {
    override def combine(x: MissingFields, y: MissingFields): MissingFields =
      MissingFields(x.fields.concat(y.fields))
  }
}
