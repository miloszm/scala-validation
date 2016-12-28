package com.ce

import cats.data.{Validated, _}
import com.ce.validation.{Err, ErrorCode}

trait DataValidator {
  def validateEmail(email: String): ValidatedNel[Err, String] = {
    val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
    email match {
      case e if emailRegex.findFirstMatchIn(e).isDefined => Validated.valid(e)
      case _ => Validated.invalidNel(Err(ErrorCode.InvalidEmailFormat, "Invalid email format"))
    }
  }

  def validatePhone(phone: String): ValidatedNel[Err, String] = {
    val phoneRegex = """^\+(?:[0-9] ?){6,14}[0-9]$""".r
    phone match {
      case p if phoneRegex.findFirstMatchIn(p).isDefined => Validated.valid(p)
      case _ => Validated.invalidNel(Err(ErrorCode.PhoneMustBeNumeric, "Phone number must be numeric"))
    }
  }

}
