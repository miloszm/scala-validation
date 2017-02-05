package com.ce.validation

import cats.data.{NonEmptyList, Validated}
import com.ce.{BasicValidator, Data}
import org.scalatest.{FlatSpec, Matchers}

class BasicValidatorSpec extends FlatSpec with Matchers {

  "Validation of data object with invalid members" should
    "contain list of validation errors" in {
    val validation = BasicValidator.validateData(Data("bad email", "bad phone"))
    val err1 = Err(ErrorCode.InvalidEmailFormat, "invalid email format")
    val err2 = Err(ErrorCode.InvalidPhoneNumberFormat, "invalid phone number format")
    val nel = NonEmptyList(err1, List(err2))
    validation shouldBe Validated.Invalid(nel)
  }

  "Validation of data object with valid members" should
    "contain data object" in {
    val data: Data = Data("good@email.com", "+447912341111")
    val validation = BasicValidator.validateData(data)
    validation shouldBe Validated.Valid(data)
  }

}
