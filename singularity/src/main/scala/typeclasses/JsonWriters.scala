package typeclasses

object JsonWriters {
  implicit val sw: JsonWriter[String] = new JsonWriter[String] {
    override def write(value: String): Json = JsString(value)
  }
  implicit val iw: JsonWriter[Int] = new JsonWriter[Int] {
    override def write(value: Int): Json = JsInt(value)
  }
}