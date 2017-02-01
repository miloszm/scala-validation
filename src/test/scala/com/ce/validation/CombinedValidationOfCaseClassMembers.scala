package com.ce.validation

import cats.data.{NonEmptyList, Validated}
import com.ce.{NonCombiningString, _}
import org.scalatest.{FlatSpec, Matchers}

class CombinedValidationOfCaseClassMembers extends FlatSpec with Matchers {

  "Validation of data object with invalid members" should
    "contain a list of all validation errors" in {
    val validation = CombineValidator.validateData(Data("bad email", "bad phone"))
    val err1 = Err(ErrorCode.InvalidEmailFormat, "Invalid email format")
    val err2 = Err(ErrorCode.PhoneMustBeNumeric, "Phone number must be numeric")
    val err3 = Err(ErrorCode.PhoneMustHaveUKCountryCode, "Phone must have UK country code")
    val nel = NonEmptyList(err1, List(err2,err3))
    validation shouldBe Validated.Invalid(nel)
  }

  "Validation of data object with invalid members (with non-combining strings)" should
    "contain a list of all validation errors" in {
    val validation = CombineImprovedValidator.validateData(MyData(NonCombiningString("bad email"), NonCombiningString("bad phone")))
    val err1 = Err(ErrorCode.InvalidEmailFormat, "Invalid email format")
    val err2 = Err(ErrorCode.EmailMustContainWordGood, "Email must contain word 'good'")
    val err3 = Err(ErrorCode.PhoneMustBeNumeric, "Phone number must be numeric")
    val err4 = Err(ErrorCode.PhoneMustHaveUKCountryCode, "Phone must have UK country code")
    validation shouldBe Validated.Invalid(List(err1, err2, err3, err4))
  }

  "Validation of data object with invalid members (with non-combining strings)" should
    "contain partial list of validation errors because andThen does not accumulate errors" in {
    val validation = AndThenValidator.validateData(MyData(NonCombiningString("bad email"), NonCombiningString("bad phone")))
    val err1 = Err(ErrorCode.InvalidEmailFormat, "Invalid email format")
    val err2 = Err(ErrorCode.PhoneMustBeNumeric, "Phone number must be numeric")
    val nel = NonEmptyList(err1, List(err2))
    validation shouldBe Validated.Invalid(nel)
  }

  "Validation of data object with valid members" should
    "contain data object with incorrect telephone number" in {
    val data = Data("good@email.com", "+447912341111")
    val validation = CombineValidator.validateData(data)
    validation shouldBe Validated.Valid(Data("good@email.com","+447912341111+447912341111"))
  }

  "Validation of data object with valid members (with non-combining strings)" should
    "contain data object with correct telephone number" in {
    val data = MyData(NonCombiningString("good@email.com"), NonCombiningString("+447912341111"))
    val validation = CombineImprovedValidator.validateData(data)
    validation shouldBe Validated.Valid(data)
  }

  "Validation of data object with valid members (with non-combining strings)" should
    "contain data object with correct telephone number (andThen implementation)" in {
    val data = MyData(NonCombiningString("good@email.com"), NonCombiningString("+447912341111"))
    val validation = AndThenValidator.validateData(data)
    validation shouldBe Validated.Valid(data)
  }

}
