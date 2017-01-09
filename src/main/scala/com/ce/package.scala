package com

import cats.kernel.Semigroup

package object ce {

  case class NonCombiningString(value:String) extends AnyVal with Semigroup[NonCombiningString] {
    override def combine(x: NonCombiningString, y: NonCombiningString): NonCombiningString = x
  }

  case class Data2(email: NonCombiningString, phone: NonCombiningString)

}
