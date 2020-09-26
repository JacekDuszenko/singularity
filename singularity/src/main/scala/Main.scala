import typeclasses.JsonWriters._
import typeclasses.{Json, JsonWriter}

object Main extends App {
  println("cats are awesome")
  val jsn: Json = toJson("test json ")
  println(jsn)

  def toJson[A](value: A)(implicit writer: JsonWriter[A]): Json = writer.write(value)
}


