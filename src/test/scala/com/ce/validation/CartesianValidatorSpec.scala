package com.ce.validation

import cats.data.{NonEmptyList, Validated}
import com.ce._
import org.scalatest.{FlatSpec, Matchers}

class CartesianValidatorSpec extends FlatSpec with Matchers {

  "Validation of data object with invalid members" should
    "contain a list of all validation errors" in {
    val validation = CartesianValidator.validateData(Data("bad email", "bad phone"))
    println(validation)
    val err1 = Err(ErrorCode.EmailMustContainKeyword, "email must contain keyword good")
    val err2 = Err(ErrorCode.InvalidEmailFormat, "invalid email format")
    val err3 = Err(ErrorCode.InvalidPhoneNumberPrefix, "phone must have prefix: +44")
    val err4 = Err(ErrorCode.InvalidPhoneNumberFormat, "invalid phone number format")
    val nel = NonEmptyList(err1, List(err2,err3,err4))
    validation shouldBe Validated.Invalid(nel)
  }

  "Validation of data object with valid members" should
    "contain data object with incorrect telephone number" in {
    val data = Data("good@email.com", "+447912341111")
    val validation = CartesianValidator.validateData(data)
    validation shouldBe Validated.Valid(Data("good@email.com","+447912341111"))
  }

}
