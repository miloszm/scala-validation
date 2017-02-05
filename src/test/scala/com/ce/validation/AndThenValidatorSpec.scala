package com.ce.validation

import cats.data.{NonEmptyList, Validated}
import com.ce.{NonCombiningString, _}
import org.scalatest.{FlatSpec, Matchers}

class AndThenValidatorSpec extends FlatSpec with Matchers {

  "Validation of data object with invalid members (with non-combining strings)" should
    "contain partial list of validation errors because andThen does not accumulate errors" in {
    val validation = AndThenValidator.validateData(Data(("bad email"), "bad phone"))
    val err1 = Err(ErrorCode.InvalidEmailFormat, "invalid email format")
    val err2 = Err(ErrorCode.InvalidPhoneNumberFormat, "invalid phone number format")
    val nel = NonEmptyList(err1, List(err2))
    validation shouldBe Validated.Invalid(nel)
  }

  "Validation of data object with valid members" should
    "contain data object with correct telephone number (andThen implementation)" in {
    val data = Data("good@email.com", "+447912341111")
    val validation = AndThenValidator.validateData(data)
    validation shouldBe Validated.Valid(data)
  }

}
