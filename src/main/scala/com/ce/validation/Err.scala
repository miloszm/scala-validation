package com.ce.validation

object ErrorCode extends Enumeration {
  type ErrorCode = Value
  val InvalidEmailFormat,
  EmailMustContainKeyword,
  InvalidPhoneNumberFormat,
  InvalidPhoneNumberPrefix = Value
}

import com.ce.validation.ErrorCode.ErrorCode
case class Err(code: ErrorCode, msg: String)
