package com.ce.validation

import cats.data.Validated
import com.ce.{NonCombiningString, _}
import org.scalatest.{FlatSpec, Matchers}

class CombineValidatorWithCollectionsSpec extends FlatSpec with Matchers {

  "Validation of data object with invalid members (with non-combining strings)" should
    "contain a list of all validation errors" in {
    val validation = CombineValidatorWithCollections.validateData(MyData(NonCombiningString("bad email"), NonCombiningString("bad phone")))
    val err1 = Err(ErrorCode.InvalidEmailFormat, "invalid email format")
    val err2 = Err(ErrorCode.EmailMustContainKeyword, "email must contain keyword good")
    val err3 = Err(ErrorCode.InvalidPhoneNumberPrefix, "phone must have prefix: +44")
    val err4 = Err(ErrorCode.InvalidPhoneNumberFormat, "invalid phone number format")
    validation shouldBe Validated.Invalid(List(err1, err2, err3, err4))
  }

  "Validation of data object with valid members (with non-combining strings)" should
    "contain data object with correct telephone number" in {
    val data = MyData(NonCombiningString("good@email.com"), NonCombiningString("+447912341111"))
    val validation = CombineValidatorWithCollections.validateData(data)
    validation shouldBe Validated.Valid(data)
  }

}
