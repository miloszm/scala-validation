package com.ce

import cats.data.{Validated, _}
import com.ce.validation.{Err, ErrorCode, Validation}

trait CombinedDataValidator {
  def validateEmailByRegex(email: NonCombiningString): Validation[NonCombiningString] = {
    val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
    email.value match {
      case e if emailRegex.findFirstMatchIn(e).isDefined => Validated.valid(NonCombiningString(e))
      case _ => Validated.invalid(List(Err(ErrorCode.InvalidEmailFormat, "invalid email format")))
    }
  }

  def validateEmailByKeyword(email: NonCombiningString, keyword:String): Validation[NonCombiningString] =
    if (email.value.toLowerCase contains keyword) Validated.valid(email)
    else Validated.invalid(List(Err(ErrorCode.EmailMustContainKeyword,
      s"email must contain keyword ${keyword}")))

  def validatePhoneByRegex(phone: NonCombiningString): Validation[NonCombiningString] = {
    val phoneRegex = """^\+(?:[0-9] ?){6,14}[0-9]$""".r
    phone.value match {
      case p if phoneRegex.findFirstMatchIn(p).isDefined => Validated.valid(NonCombiningString(p))
      case _ => Validated.invalid(List(Err(ErrorCode.InvalidPhoneNumberFormat,
        s"invalid phone number format")))
    }
  }

  def validatePhoneByPrefix(phone: NonCombiningString, prefix:String): Validation[NonCombiningString] =
    if (phone.value contains prefix) Validated.valid(phone)
    else Validated.invalid(List(Err(ErrorCode.InvalidPhoneNumberPrefix,
      s"phone must have prefix: ${prefix}")))
}
