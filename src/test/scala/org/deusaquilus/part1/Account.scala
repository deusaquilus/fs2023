package org.deusaquilus.part1

import org.deusaquilus.part1.AccountNumber

//enum AccountNumber {
//  case AccountNumber(value: Int)
//}
//
//enum CustomerId {
//  case CustomerId(value: Int)
//}

class AccountNumber(val value: Int) extends AnyVal
class CustomerId(val value: Int) extends AnyVal

case class Account(
  accountNumber: AccountNumber,
  //customerId: CustomerId,
  numOrders: Int
)
