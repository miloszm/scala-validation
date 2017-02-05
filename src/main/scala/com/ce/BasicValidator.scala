package com.ce

import cats.data._
import cats.implicits._
import com.ce.validation.Err


object BasicValidator extends DataValidator {

  def validateData(d: Data): ValidatedNel[Err, Data] = {
    val validEmail = validateEmailByRegex(d.email)
    val validPhone = validatePhoneByRegex(d.phone)

    (validEmail |@| validPhone).map(Data)
  }

  /**
    * problem: works fine only for one validation per case class member
    */

}

