package org.deusaquilus.part2.pattern1_adapter

import zio.json.*

object ZioJsonExample {

  case class UserId(value: String)
  case class Person(id: UserId, firstName: String, lastName: String)

  def main(args: Array[String]): Unit = {

    // implicit val userIdEncoder: JsonEncoder[UserId] =
    //   BuildJsonEncoder.gen[UserId]

    implicit val userIdEncoder: JsonEncoder[UserId] =
      JsonEncoder.string.contramap(
        (id: UserId) => id.value
      )

    implicit val personEncoder: JsonEncoder[Person] =
      DeriveJsonEncoder.gen[Person]

    val id = UserId("123")
    val person = Person(id, "John", "Doe")

    println(person.toJson)
  }
}
