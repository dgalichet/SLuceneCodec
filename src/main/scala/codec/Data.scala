package codec

sealed trait Data

// Indexed, analyzed
case class IndexedText(value: String) extends Data

// Indexed, not analyzed
case class IndexedString(value: String) extends Data

// Indexed, not stored fields
case class IndexedInt(value: Int) extends Data
case class IndexedLong(value: Long) extends Data
case class IndexedFloat(value: Float) extends Data
case class IndexedDouble(value: Double) extends Data

