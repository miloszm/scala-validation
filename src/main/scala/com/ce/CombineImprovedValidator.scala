package com.ce

import cats.Semigroup
import cats.data._
import cats.implicits._
import com.ce.validation.{Err, ErrorCode, Validation}

object CombineImprovedValidator extends App {
  implicit val nonCombiningStringSemigroup = Semigroup(NonCombiningString(""))

  def validateEmailByRegex(email: NonCombiningString): Validation[NonCombiningString] = {
    val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
    email.value match {
      case emailRegex(e) => Validated.valid(NonCombiningString(e))
      case _ => Validated.invalid(List(Err(ErrorCode.InvalidEmailFormat, "invalid email format")))
    }
  }

  def validateEmailByKeyword(email: NonCombiningString, keyword:String): Validation[NonCombiningString] =
    if (email.value.toLowerCase contains keyword) Validated.valid(email)
    else Validated.invalid(List(Err(ErrorCode.EmailMustContainWordGood,
      s"email must contain keyword ${keyword}")))

  def validatePhoneByRegex(phone: NonCombiningString): Validation[NonCombiningString] = {
    val phoneRegex = """^\+(?:[0-9] ?){6,14}[0-9]$""".r
    phone.value match {
      case phoneRegex(p) => Validated.valid(NonCombiningString(p))
      case _ => Validated.invalid(List(Err(ErrorCode.PhoneMustBeNumeric,
        s"invalid phone number format")))
    }
  }

  def validatePhoneByPrefix(phone: NonCombiningString, prefix:String): Validation[NonCombiningString] =
    if (phone.value contains prefix) Validated.valid(phone)
    else Validated.invalid(List(Err(ErrorCode.PhoneMustHaveUKCountryCode,
      s"phone must have prefix: ${prefix}")))

  def validateData(d: MyData): Validation[MyData] = {
    val validEmail = validateEmailByRegex(d.email)
      .combine(validateEmailByKeyword(d.email, "good"))

    val validPhone = validatePhoneByRegex(d.phone)
      .combine(validatePhoneByPrefix(d.phone, "+44"))

    (validEmail |@| validPhone).map(MyData)
  }

  val v = validateData(MyData(NonCombiningString("wrong email"),
    NonCombiningString("wrong phone number")))
  v.leftMap{ e =>
    e.foreach(ee => println(ee.msg))
  }
}

