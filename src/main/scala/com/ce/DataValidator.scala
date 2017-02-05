package com.ce

import cats.data.{Validated, _}
import com.ce.validation.{Err, ErrorCode}

trait DataValidator {
  def validateEmailByRegex(email: String): ValidatedNel[Err, String] = {
    val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
    email match {
      case e if emailRegex.findFirstMatchIn(e).isDefined => Validated.valid(e)
      case _ => Validated.invalidNel(Err(ErrorCode.InvalidEmailFormat, "invalid email format"))
    }
  }

  def validateEmailByKeyword(email: String, keyword:String): ValidatedNel[Err, String] =
    if (email.toLowerCase contains keyword) Validated.valid(email)
    else Validated.invalidNel(Err(ErrorCode.EmailMustContainKeyword,
      s"email must contain keyword ${keyword}"))

  def validatePhoneByRegex(phone: String): ValidatedNel[Err, String] = {
    val phoneRegex = """^\+(?:[0-9] ?){6,14}[0-9]$""".r
    phone match {
      case p if phoneRegex.findFirstMatchIn(p).isDefined => Validated.valid(p)
      case _ => Validated.invalidNel(Err(ErrorCode.InvalidPhoneNumberFormat,
        s"invalid phone number format"))
    }
  }

  def validatePhoneByPrefix(phone: String, prefix:String): ValidatedNel[Err, String] =
    if (phone contains prefix) Validated.valid(phone)
    else Validated.invalidNel(Err(ErrorCode.InvalidPhoneNumberPrefix,
      s"phone must have prefix: ${prefix}"))

}
