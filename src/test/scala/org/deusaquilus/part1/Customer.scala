package org.deusaquilus.part1

// This CANNOT be in the same class as the thing that uses e.e. in BuilderEnum otherwise
// the macro-summoning mechanism will not recognize Person/Robot as Product types
enum Customer {
  case Person(firstName: String, lastName: String)
  case Robot(id: Int)
  case Unknown
}

//val person = Customer.Person("Joe", "Bloggs")

