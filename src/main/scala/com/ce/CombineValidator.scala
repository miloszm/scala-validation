package com.ce

import cats.data._
import cats.implicits._
import com.ce.validation.{Err, ErrorCode}


object CombineValidator extends App with DataValidator {

  def validateData(d: Data): ValidatedNel[Err, Data] = {
    val validEmailFormat = validateEmailByRegex(d.email)
    val validEmail = validateEmailByKeyword(d.email, "good").combine(validEmailFormat)
    val validPhoneFormat = validatePhoneByPrefix(d.phone, "+44")
    val validPhone = validatePhoneByRegex(d.phone).combine(validPhoneFormat)

    (validEmail |@| validPhone).map(Data)
  }

  println(validateData(Data("bad", "bad")))

  /**
    * that is fine, errors are accumulated correctly,
    */

  println(validateData(Data("good@email.com", "+447912341111")))

  /**
    * but in correct case we get multiple values concatenated
    * Valid(Data(good@email.comgood@email.com,+447912341111+447912341111))
    */

}

