package parsec.gui.grammar

import scala.util.parsing.combinator.debugging._
import parsec.gui.Utils


import parsec.gui.parsingTree.ParsingStatus._

trait Grammar{
  def highlight(name: String, loc: ParserLocation, status: ParsingStatus): Unit
}

class GrammarNotifier extends Listener {
  private[this] var grammars: List[Grammar] = Nil
  
  def addGrammar(grammar: Grammar) = grammars = grammar::grammars

  def stepIn(id: Int, name: String, loc: ParserLocation): Option[Notification] = {
    grammars.map (_ highlight(name, loc, UNKNOWN))
    None
  }

  def stepOut(id: Int, success: Boolean, msg: String): Option[Notification] = {
    //grammars.map (_ highlight(name, loc, msg match {}))
    None
  }

}