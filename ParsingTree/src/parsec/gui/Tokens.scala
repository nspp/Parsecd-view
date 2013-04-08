package parsec.gui

import java.util.Collections

object TokenStatus extends Enumeration {
  type TokenStatus = Value
  val UNKNOWN, PARSED, PENDING = Value
}


import TokenStatus._
sealed class Tokens(name: String, tokenStatus: TokenStatus = PENDING) {
  var status = tokenStatus
  override def toString = name
}