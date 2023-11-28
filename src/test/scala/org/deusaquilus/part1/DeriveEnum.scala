package org.deusaquilus.part1

import org.deusaquilus.part1.Customer
import org.deusaquilus.part1.builder.*
import org.deusaquilus.part1.print.DetailLevel
//import org.deusaquilus.part1.builder.Builder.{given, *}
import org.deusaquilus.part1.print.PrintMDM

object BuildEnum {
  def main(args: Array[String]): Unit = {
    // Need to annotate with `:Customer"
    val customer: Customer = Customer.Person("Joe", "Bloggs")

    // BuilderMacros.errorOnType[T]("Attempting to builder a product")
    val d = DataBuilder.gen[Customer] // // // // // // // // // // //  //

//    val data = Builder.toData(customer)
    println(PrintMDM(d.build(customer), DetailLevel.Detailed))
  }
}
