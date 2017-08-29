package codec

import java.time.LocalDateTime

import org.apache.lucene.document._
import cats.syntax.traverse._
import cats.instances.list._
import cats.syntax.validated._
import cats.syntax.cartesian._
import org.apache.lucene.index.IndexableField

import scala.annotation.implicitNotFound
import scala.util.Try


@implicitNotFound("Cannot find Encoder for instance ${A}.\nPlease make sure that there exists encoders for all compound types used in type ${A}")
sealed trait FieldsCodec[A] {
  def encode(name: String, a: A): List[Field]
  def decode(name: String, fs: List[IndexableField]): DecodedResult[A]
}

object FieldsCodec {
  import shapeless._
  import shapeless.labelled.FieldType

  private def createCodec[A](f: (String, A) ⇒ Field, g: (String, IndexableField) ⇒ Option[A]): FieldsCodec[A] = new FieldsCodec[A] {
    override def encode(name: String, a: A): List[Field] = List(f(name, a))
    override def decode(name: String, fs: List[IndexableField]): DecodedResult[A] = fs.find(_.name() == name).flatMap(g(name, _)) match {
      case Some(a) ⇒ a.valid
      case None ⇒ MissingFields(name).invalid
    }
  }

  // Indexed, stored field, analyzed
  implicit val IndexedTextToField: FieldsCodec[IndexedText] = createCodec(
    { (name, a) ⇒ new TextField(name, a.value, Field.Store.YES) },
    { (_, f) ⇒ Some(IndexedText(f.stringValue)) }
  )

  // Indexed, stored field, not analyzed
  implicit val IndexedStringToField: FieldsCodec[IndexedString] = createCodec (
    { (name, a) ⇒ new StringField(name, a.value, Field.Store.YES) },
    { (_, f) ⇒ Some(IndexedString(f.stringValue())) }
  )

  // Indexed, not stored field
  implicit val IndexedIntToField: FieldsCodec[IndexedInt] = createCodec (
    { (name, a) ⇒ new IntPoint(name, a.value) },
    { (_, a) ⇒ Try(IndexedInt(a.numericValue().intValue())).toOption }
  )
  implicit val IndexedLongToField: FieldsCodec[IndexedLong] = createCodec (
    { (name, a) ⇒ new LongPoint(name, a.value) },
    { (_, a) ⇒ Try(IndexedLong(a.numericValue().longValue())).toOption }
  )
  implicit val IndexedFloatToField: FieldsCodec[IndexedFloat] = createCodec (
    { (name, a) ⇒ new FloatPoint(name, a.value) },
    { (_, a) ⇒ Try(IndexedFloat(a.numericValue().floatValue())).toOption }
  )
  implicit val IndexedDoubleToField: FieldsCodec[IndexedDouble] = createCodec (
    { (name, a) ⇒ new DoublePoint(name, a.value) },
    { (_, a) ⇒ Try(IndexedDouble(a.numericValue().doubleValue())).toOption }
  )

  // Not indexed, stored fields
  implicit val StringToField: FieldsCodec[String] = createCodec (
    { (name, a) ⇒ new StoredField(name, a) },
    { (_, a) ⇒ Try(a.stringValue()).toOption }
  )
  implicit val IntToField: FieldsCodec[Int] = createCodec (
    { (name, a) ⇒ new StoredField(name, a) },
    { (_, a) ⇒ Try(a.numericValue().intValue()).toOption }
  )
  implicit val LongToField: FieldsCodec[Long] = createCodec (
    { (name, a) ⇒ new StoredField(name, a) },
    { (_, a) ⇒ Try(a.numericValue().longValue()).toOption }
  )
  implicit val FloatToField: FieldsCodec[Float] = createCodec (
    { (name, a) ⇒ new StoredField(name, a) },
    { (_, a) ⇒ Try(a.numericValue().floatValue()).toOption }
  )
  implicit val DoubleToField: FieldsCodec[Double] = createCodec (
    { (name, a) ⇒ new StoredField(name, a) },
    { (_, a) ⇒ Try(a.numericValue().doubleValue()).toOption }
  )

  implicit val BooleanToField: FieldsCodec[Boolean] = createCodec (
    { (name, a) ⇒ new StoredField(name, if (a) "true" else "false") },
    { (_, a) ⇒ Some(if (a.stringValue() == "true") true else false )}
  )

  implicit val LocalDateTimeField: FieldsCodec[LocalDateTime] = createCodec (
    { (name, a) ⇒ new StoredField(name, a.toString) },
    { (_, a) ⇒ Try(LocalDateTime.parse(a.stringValue())).toOption }
  )

  implicit def optionEncoder[A](implicit codec: FieldsCodec[A]): FieldsCodec[Option[A]] = new FieldsCodec[Option[A]] {
    override def encode(name: String, oa: Option[A]): List[Field] = oa match {
      case Some(a) ⇒ codec.encode(name, a)
      case None ⇒ Nil
    }

    override def decode(name: String, fs: List[IndexableField]): DecodedResult[Option[A]] = {
      codec.decode(name, fs).toOption.valid
    }
  }

  implicit def listEncoder[A](implicit codec: FieldsCodec[A]): FieldsCodec[List[A]] = new FieldsCodec[List[A]] {
    override def encode(name: String, as: List[A]): List[Field] =
      as.zipWithIndex.flatMap { case (a, i) ⇒ codec.encode(s"${name}_$i", a)}

    override def decode(name: String, fs: List[IndexableField]): DecodedResult[List[A]] = {
      fs.filter(_.name.startsWith(name))
        .groupBy(_.name().substring(name.length + 1).split('_').head.toInt)
        .toList
        .traverse[DecodedResult, A] { case (i, l) ⇒
          codec.decode(s"${name}_$i", l)
      }
    }
  }

  implicit def setEncoder[A](implicit codec: FieldsCodec[A]): FieldsCodec[Set[A]] = new FieldsCodec[Set[A]] {
    override def encode(name: String, as: Set[A]): List[Field] =
      as.toList.zipWithIndex.flatMap { case (a, i) ⇒ codec.encode(s"${name}_$i", a)}

    override def decode(name: String, fs: List[IndexableField]): DecodedResult[Set[A]] =
      fs.filter(_.name.startsWith(name))
        .groupBy(_.name().substring(name.length + 1).split('_').head.toInt)
        .toList
        .traverse[DecodedResult, A] { case (i, l) ⇒
          codec.decode(s"${name}_$i", l)
        }.map(_.toSet)
  }

  implicit val hnilEncoder: FieldsCodec[HNil] = new FieldsCodec[HNil] {
    override def encode(name: String, a: HNil): List[Field] = Nil

    override def decode(name: String, fs: List[IndexableField]): DecodedResult[HNil] = HNil.valid
  }

  implicit def hlistEncoder[K <: Symbol, H, T <: HList]( implicit
                                                         witness: Witness.Aux[K],
                                                         hCodec: Lazy[FieldsCodec[H]],
                                                         tCodec: FieldsCodec[T]
                                                       ): FieldsCodec[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    new FieldsCodec[FieldType[K, H] :: T] {
      override def encode(name: String, a: FieldType[K, H] :: T): List[Field] = a match {
        case h :: t ⇒ hCodec.value.encode(s"${name}_$fieldName", h) ::: tCodec.encode(name, t)
      }

      override def decode(name: String, fs: List[IndexableField]): DecodedResult[FieldType[K, H] :: T] = {
        (hCodec.value.decode(s"${name}_$fieldName", fs) |@| tCodec.decode(name, fs.filterNot(_.name == name)))
          .map { (h, t) ⇒
            val kh = labelled.field[K](h)
            kh :: t
          }
      }
    }
  }

  implicit def genericObjectEncoder[A, R]( implicit
                                           generic: LabelledGeneric.Aux[A, R],
                                           codec: FieldsCodec[R]
                                         ): FieldsCodec[A] =
    new FieldsCodec[A] {
      override def encode(name: String, a: A): List[Field] = codec.encode(name, generic.to(a))
      override def decode(name: String, fs: List[IndexableField]): DecodedResult[A] = codec.decode(name, fs).map(generic.from)
    }


  def apply[A](implicit encoder: FieldsCodec[A]): FieldsCodec[A] = encoder
}


