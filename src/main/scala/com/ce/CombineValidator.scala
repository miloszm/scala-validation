package com.ce

import cats.data._
import cats.implicits._
import com.ce.validation.{Err, ErrorCode}


object CombineValidator extends DataValidator {
  def validateUKCountryCode(phone: String): ValidatedNel[Err, String] =
    if (phone contains "+44") Validated.valid(phone) else Validated.invalidNel(Err(ErrorCode.PhoneMustHaveUKCountryCode, "Phone must have UK country code"))

  def perform3rdPartyValidations(phone: String): List[ValidatedNel[Err, String]] =
    List(validatePhone(phone),validateUKCountryCode(phone))

  def validateData(d: Data): ValidatedNel[Err, Data] = {
    val validEmail = validateEmail(d.email)
    val validPhoneFormat = validatePhone(d.phone)
    val validPhoneCountry = validateUKCountryCode(d.phone)
    val validPhone = validPhoneFormat.combine(validPhoneCountry)

    (validEmail |@| validPhone).map(Data)
  }

}

