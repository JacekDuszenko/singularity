package typeclasses

object JsonWriters {
  implicit val sw: JsonWriter[String] = (value: String) => JsString(value)
  implicit val iw: JsonWriter[Int]    = (value: Int) => JsInt(value)
}
