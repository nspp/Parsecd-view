package parsec.gui.listeners

import scala.util.parsing.combinator.debugging.Listener
import scala.util.parsing.combinator.debugging.ParserLocation
import scala.util.parsing.combinator.debugging.Notification
//import scala.util.parsing.combinator.Parsers.ParseResult

class TokenListener[T] extends Listener {
  var tokens: List[T] = Nil

  def stepIn(id: Int, name: String, loc: ParserLocation): Option[Notification] = None

  def stepOut(id: Int, success: Boolean, msg: String): Option[Notification] = None

  /*override def stepOut(id: Int, res: ParseResult[U], msg: String): Option[Notification] = {
   res  match {
     case Success(_,_) => None
     case _ => None
   }
  }*/
}