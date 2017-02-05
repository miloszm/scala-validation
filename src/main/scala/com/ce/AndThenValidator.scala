package com.ce

import com.ce.validation.Err
import cats.data._
import cats.implicits._

object AndThenValidator extends DataValidator {

  def validateData(d: Data): ValidatedNel[Err, Data] = {
    val validEmail = validateEmailByRegex(d.email).andThen(validateEmailByKeyword(_, "good"))
    val validPhone = validatePhoneByRegex(d.phone).andThen(validatePhoneByPrefix(_, "+44"))

    (validEmail |@| validPhone).map(Data)
  }

  /**
    * problem: only first failed validations are captured
    */

}

