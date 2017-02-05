package com.ce

import cats._
import cats.data._
import cats.implicits._
import com.ce.validation.{Err, Validation}


object CombineValidatorImproved extends CombinedDataValidator {

  implicit val nonCombiningStringSemigroup = Semigroup(NonCombiningString(""))

  def validateData(d: MyData): Validation[MyData] = {
    val validEmailFormat = validateEmailByRegex(d.email)
    val validEmail = validateEmailByKeyword(d.email, "good").combine(validEmailFormat)
    val validPhoneFormat = validatePhoneByPrefix(d.phone, "+44")
    val validPhone = validatePhoneByRegex(d.phone).combine(validPhoneFormat)

    (validEmail |@| validPhone).map(MyData)
  }

//  println(validateData(MyData("bad", "bad")))

  /**
    * that is fine, errors are accumulated correctly,
    */

//  println(validateData(MyData("good@email.com", "+447912341111")))

  /**
    * also fine
    * values accumulated correctly, no multiple concatenation
    */

}

