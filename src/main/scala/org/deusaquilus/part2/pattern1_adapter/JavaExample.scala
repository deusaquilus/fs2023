package org.deusaquilus.part2.pattern1_adapter

import com.google.gson.{GsonBuilder, JsonPrimitive, JsonSerializationContext, JsonSerializer}

import java.lang.reflect.Type
import scala.beans.BeanProperty

object JavaExample {

  case class UserIdBean(@BeanProperty value: String) extends AnyVal

  class UserId(val value: String) extends AnyVal

  class UserIdAdapter extends JsonSerializer[UserId] {
    def serialize(src: UserId, typeOfSrc: Type, context: JsonSerializationContext) =
      new JsonPrimitive(src.value)
  }

  def main(args: Array[String]): Unit = {
    val builder = new GsonBuilder()
    builder.registerTypeAdapter(classOf[UserId], new UserIdAdapter())
    val gson = builder.create()

    // val userIdBean = UserIdBean("123")
    // println(gson.toJson(userId)) // {"value":"123"}

    val userId = new UserId("123")
    println(gson.toJson(userId))
  }
}
