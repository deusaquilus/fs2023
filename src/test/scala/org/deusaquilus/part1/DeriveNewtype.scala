package org.deusaquilus.part1

import org.deusaquilus.part1.builder.{CustomDataBuilder, DataBuilder}
import org.deusaquilus.part1.newtypes.MakeNewtypeEnum
import org.deusaquilus.part1.print.{DetailLevel, PrintMDM}
import org.deusaquilus.part1.{Schema, Data}
import org.deusaquilus.part1.{Account, AccountNumber}

object BuildNewtype0 {

  implicit val accountNumBuilder: CustomDataBuilder[AccountNumber] =
    new CustomDataBuilder[AccountNumber] {
      val wrapper = MakeNewtypeEnum("AccountNumber", Schema.Int)
      override def build(value: AccountNumber): Data.Enum =
        wrapper.construct(Data.Int(value.value))
      override def schema: Schema.Enum =
        wrapper.schema
    }

  val acctNum = AccountNumber(123)
  val custId = CustomerId(456)
  val joeAccount = Account(acctNum, /*custId,*/ 550)

  def main(args: Array[String]): Unit = {
    val d = DataBuilder.build[AccountNumber](acctNum)
    println(PrintMDM(d, DetailLevel.Detailed))
  }
}

object BuildNewtype1 {
  import org.deusaquilus.part1.newtypes.approach1_boilerplate.*

  implicit val accountNumBuilder: CustomDataBuilder[AccountNumber] =
    NewtypeEnum.build[AccountNumber](_.value)

  val acctNum = AccountNumber(123)
  val custId = CustomerId(456)
  val joeAccount = Account(acctNum, /*custId,*/ 550)

  def main(args: Array[String]): Unit = {
    val d = DataBuilder.build[AccountNumber](acctNum)
    println(PrintMDM(d, DetailLevel.Detailed))
  }
}

object BuildNewtype2 {
  import org.deusaquilus.part1.newtypes.approach2_typeclasses.*

  implicit val accountNumBuilder: CustomDataBuilder[AccountNumber] =
    NewtypeEnum.derive[AccountNumber, Data.Int, Int](_.value)

  val acctNum = AccountNumber(123)
  val custId = CustomerId(456)
  val joeAccount = Account(acctNum, /*custId,*/ 550)

  def main(args: Array[String]): Unit = {
    val d = DataBuilder.build[AccountNumber](acctNum)
    println(PrintMDM(d, DetailLevel.Detailed))
  }
}

object BuildNewtype3 {
  import org.deusaquilus.part1.newtypes.approach3_typeclasses.*

  implicit val accountNumBuilder: CustomDataBuilder[AccountNumber] =
    NewtypeEnum[AccountNumber].derive(_.value)

  val acctNum = AccountNumber(123)
  val custId = CustomerId(456)
  val joeAccount = Account(acctNum, /*custId,*/ 550)

  def main(args: Array[String]): Unit = {
    val d = DataBuilder.build[AccountNumber](acctNum)
    println(PrintMDM(d, DetailLevel.Detailed))
  }
}


object BuildNewtype4 {
  import org.deusaquilus.part1.newtypes.approach4_typeclasses.*

  implicit val accountNumBuilder: CustomDataBuilder[AccountNumber] =
    NewtypeEnum[AccountNumber].derive(_.value)

  val acctNum = AccountNumber(123)
  val custId = CustomerId(456)
  val joeAccount = Account(acctNum, /*custId,*/ 550)

  def main(args: Array[String]): Unit = {
    val d = DataBuilder.build[AccountNumber](acctNum)
    println(PrintMDM(d, DetailLevel.Detailed))
  }
}
