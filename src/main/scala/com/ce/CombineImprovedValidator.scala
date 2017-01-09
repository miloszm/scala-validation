package com.ce

import cats.Semigroup
import cats.data._
import cats.implicits._
import com.ce.validation.{Err, ErrorCode, Validation}

object CombineImprovedValidator {
  implicit val nonCombiningStringSemigroup = Semigroup(NonCombiningString(""))

  def validateEmail1(email: NonCombiningString): Validation[NonCombiningString] = {
    val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
    email match {
      case e if emailRegex.findFirstMatchIn(e.value).isDefined => Validated.valid(e)
      case _ => Validated.invalid(List(Err(ErrorCode.InvalidEmailFormat, "Invalid email format")))
    }
  }

  def validateEmail2(email: NonCombiningString): Validation[NonCombiningString] =
    if (email.value.toLowerCase contains "good") Validated.valid(email) else Validated.invalid(List(Err(ErrorCode.EmailMustContainWordGood, "Email must contain word 'good'")))

  def validatePhone1(phone: NonCombiningString): Validation[NonCombiningString] = {
    val phoneRegex = """^\+(?:[0-9] ?){6,14}[0-9]$""".r
    phone match {
      case p if phoneRegex.findFirstMatchIn(p.value).isDefined => Validated.valid(p)
      case _ => Validated.invalid(List(Err(ErrorCode.PhoneMustBeNumeric, "Phone number must be numeric")))
    }
  }

  def validatePhone2(phone: NonCombiningString): Validation[NonCombiningString] =
    if (phone.value contains "+44") Validated.valid(phone) else Validated.invalid(List(Err(ErrorCode.PhoneMustHaveUKCountryCode, "Phone must have UK country code")))

  def validateData(d: Data2): Validation[Data2] = {
    val validEmail = validateEmail1(d.email).combine(validateEmail2(d.email))
    val validPhone = validatePhone1(d.phone).combine(validatePhone2(d.phone))
    (validEmail |@| validPhone).map(Data2)
  }

}

