package com.ce

import cats._
import cats.implicits._
import com.ce.validation.Validation


object CombineValidatorWithCollections extends CombinedDataValidator {

  implicit val nonCombiningStringSemigroup = Semigroup(NonCombiningString(""))

  def validateData(d: MyData): Validation[MyData] = {
    val emailValidations = List(validateEmailByRegex(d.email), validateEmailByKeyword(d.email, "good"))
    val validEmail = emailValidations.reduceLeft(_ combine _)
    val phoneValidations = List(validatePhoneByPrefix(d.phone, "+44"), validatePhoneByRegex(d.phone))
    val validPhone = phoneValidations.reduceLeft(_ combine _)

    (validEmail |@| validPhone).map(MyData)
  }

}

