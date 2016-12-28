package com.ce

import cats.data._
import cats.implicits._
import com.ce.validation.{Err, ErrorCode}


object BasicValidator extends DataValidator {

  def validateData(d: Data): ValidatedNel[Err, Data] = {
    val validEmail = validateEmail(d.email)
    val validPhone = validatePhone(d.phone)

    (validEmail |@| validPhone).map(Data)
  }

}

