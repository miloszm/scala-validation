package com.ce

import cats._
import cats.data._
import cats.implicits._
import cats.Semigroup
import com.ce.validation.{Err, ErrorCode, Validation}

object CombineImprovedValidator {
//  object ErrorListSemigroup extends Semigroup[List[Err]] {
//    override def combine(x: List[Err], y: List[Err]): List[Err] = x ::: y
//  }
//
//  implicit val errorListSemigroup = Semigroup(ErrorListSemigroup)

  implicit val nonCombiningStringSemigroup = Semigroup(NonCombiningString(""))

  def validateEmail(email: NonCombiningString): Validation[NonCombiningString] = {
    val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
    email match {
      case e if emailRegex.findFirstMatchIn(e.value).isDefined => Validated.valid(e)
      case _ => Validated.invalid(List(Err(ErrorCode.InvalidEmailFormat, "Invalid email format")))
    }
  }

  def validatePhone(phone: NonCombiningString): Validation[NonCombiningString] = {
    val phoneRegex = """^\+(?:[0-9] ?){6,14}[0-9]$""".r
    phone match {
      case p if phoneRegex.findFirstMatchIn(p.value).isDefined => Validated.valid(p)
      case _ => Validated.invalid(List(Err(ErrorCode.PhoneMustBeNumeric, "Phone number must be numeric")))
    }
  }


  def validateEmailDomain(email: NonCombiningString): Validation[NonCombiningString] =
    if (email.value.toLowerCase contains "good") Validated.valid(email) else Validated.invalid(List(Err(ErrorCode.EmailMustContainWordGood, "Email must contain word 'good'")))

  def validateUKCountryCode(phone: NonCombiningString): Validation[NonCombiningString] =
    if (phone.value contains "+44") Validated.valid(phone) else Validated.invalid(List(Err(ErrorCode.PhoneMustHaveUKCountryCode, "Phone must have UK country code")))

  def validateData(d: Data3): Validation[Data3] = {
    val validEmail = validateEmail(d.email).combine(validateEmailDomain(d.email))
    val validPhoneFormat = validatePhone(d.phone)
    val validPhoneCountry = validateUKCountryCode(d.phone)
    val validPhone = validPhoneFormat.combine(validPhoneCountry)

    (validEmail |@| validPhone).map(Data3)
  }

}

