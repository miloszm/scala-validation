package com

import cats.kernel.Semigroup

package object ce {

  case class NonCombiningString(value:String) extends AnyVal with Semigroup[NonCombiningString] {
    override def combine(x: NonCombiningString, y: NonCombiningString): NonCombiningString = x
  }

  implicit def string2NonCombiningString(s:String):NonCombiningString = NonCombiningString(s)

  case class MyData(email: NonCombiningString, phone: NonCombiningString)

}
