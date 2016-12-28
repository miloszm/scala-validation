package com.ce

import cats.data.Validated

package object validation {

  type Validation[T] = Validated[List[Err], T]

}
