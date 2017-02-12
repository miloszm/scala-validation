package com.ce

import cats.data.ValidatedNel
import cats.implicits._
import com.ce.validation.Err


object CartesianValidatorWithCollections extends DataValidator {

  def validateData(d: Data): ValidatedNel[Err, Data] = {
    val emailValidations = List(validateEmailByRegex(d.email), validateEmailByKeyword(d.email, "good"))
    val validEmail: ValidatedNel[Err, String] = emailValidations.reduceLeft((a, b) => (a |@| b).map { case _ => d.email })
    val phoneValidations = List(validatePhoneByPrefix(d.phone, "+44"), validatePhoneByRegex(d.phone))
    val validPhone: ValidatedNel[Err, String] = phoneValidations.reduceLeft((a, b) => (a |@| b).map { case _ => d.phone })

    (validEmail |@| validPhone).map(Data)
  }

}

