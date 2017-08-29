package model

import codec.{FieldsCodec, IndexedInt, IndexedString, IndexedText}

case class Person(firstName: String,
                  lastName: String,
                  description: IndexedString,
                  age: Int,
                  address: Address
                 )

case class Address(
                    street: IndexedText,
                    //street2: String, // if we uncomment, we get a compilation error on personCodec (addressCodec will work fine)
                    zipCode: IndexedInt)

object Person {
  val addressCodec: FieldsCodec[Address] = implicitly[FieldsCodec[Address]]
  val personCodec: FieldsCodec[Person] = implicitly[FieldsCodec[Person]]
}