package com.ce

import cats.data._
import cats.implicits._
import com.ce.validation.{Err, ErrorCode, Validation}


object CollectionsValidator extends App {

  def validateEmailByRegex(email: String): ValidatedNel[Err, List[String]] = {
    val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
    email match {
      case e if emailRegex.findFirstMatchIn(e).isDefined => Validated.valid(List(e))
      case e => Validated.invalidNel(Err(ErrorCode.InvalidEmailFormat, s"${e} - invalid email format"))
    }
  }

  def validateEmails(emails: List[String]): ValidatedNel[Err, List[String]] = {
    val validations = emails.map(validateEmailByRegex(_))
    validations.reduceLeft(_ combine _)
  }

  println(validateEmails(List("good1@email.com", "good2@email.com")))
  // Valid(List(good1@email.com, good2@email.com))

  println(validateEmails(List("good1@email.com", "bad1", "bad2")))
  // Invalid(NonEmptyList(Err(InvalidEmailFormat,bad1 - invalid email format), Err(InvalidEmailFormat,bad2 - invalid email format)))

}

