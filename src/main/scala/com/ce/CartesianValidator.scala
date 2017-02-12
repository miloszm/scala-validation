package com.ce

import cats.data._
import cats.implicits._
import com.ce.validation.Err


object CartesianValidator extends App with DataValidator {

  def validateEmail(e:String):ValidatedNel[Err, String] =
    (validateEmailByKeyword(e, "good") |@| validateEmailByRegex(e)) map {case _ => e}

  def validatePhone(p:String):ValidatedNel[Err, String] =
    (validatePhoneByPrefix(p, "+44") |@| validatePhoneByRegex(p)) map {case _ => p}

  def validateData(d: Data): ValidatedNel[Err, Data] = {
    val validEmail = validateEmail(d.email)
    val validPhone = validatePhone(d.phone)

    val x = (validEmail |@| validPhone)
    println(x)

    (validEmail |@| validPhone).map{Data}
  }

  println(validateData(Data("bad", "bad")))

  /**
    * that is fine, errors are accumulated correctly,
    */

  println(validateData(Data("good@email.com", "+447912341111")))

  /**
    * in correct case we get
    * Valid(Data(good@email.com,+447912341111))
    */

}

