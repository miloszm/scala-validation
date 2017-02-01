package com.ce

import cats._
import cats.data._
import cats.implicits._
import cats.kernel.Semigroup
import com.ce.validation.{Err, ErrorCode}

object AndThenValidator {

  implicit val nonCombiningStringSemigroup = Semigroup(NonCombiningString(""))

  def validateEmailFormat(email: NonCombiningString): ValidatedNel[Err, NonCombiningString] = {
    val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
    email match {
      case e@NonCombiningString(v) if emailRegex.findFirstMatchIn(v).isDefined => Validated.valid(e)
      case _ => Validated.invalidNel(Err(ErrorCode.InvalidEmailFormat, "Invalid email format"))
    }
  }

  def validateEmailDomain(email: NonCombiningString): ValidatedNel[Err, NonCombiningString] = {
    if (email.value.toLowerCase contains "good") Validated.valid(email) else Validated.invalidNel(Err(ErrorCode.EmailMustContainWordGood, "Email must contain word 'good'"));
  }

  def validatePhone(phone: NonCombiningString): ValidatedNel[Err, NonCombiningString] = {
    val phoneRegex = """^\+(?:[0-9] ?){6,14}[0-9]$""".r
    phone match {
      case p@NonCombiningString(v) if phoneRegex.findFirstMatchIn(v).isDefined => Validated.valid(p)
      case _ => Validated.invalidNel(Err(ErrorCode.PhoneMustBeNumeric, "Phone number must be numeric"))
    }
  }

  def validateUKCountryCode(phone: NonCombiningString): ValidatedNel[Err, NonCombiningString] =
    if (phone.value contains "+44") Validated.valid(phone) else Validated.invalidNel(Err(ErrorCode.PhoneMustHaveUKCountryCode, "Phone must have UK country code"))

  def validateData(d: MyData): ValidatedNel[Err, MyData] = {
    val validEmail = validateEmailFormat(d.email).andThen(validateEmailDomain)
    val validPhone = validatePhone(d.phone).andThen(validateUKCountryCode)

    (validEmail |@| validPhone).map(MyData)
  }

}

