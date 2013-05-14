import scala.util.parsing.combinator._

import scala.util.parsing.combinator.debugging
import debugging.ParserMacros._  

class JSON extends JavaTokenParsers  with debugging.DebugableParsers {

  def runMain() : Unit = {
    ParseJSON.main(Array("address-book.json"))
  }   

  def value(implicit loc0: debugging.ParserLocation) : Parser[Any] = obj | arr | 
                            stringLiteral | 
                            floatingPointNumber | 
                            "null" | "true" | "false"

  def obj(implicit loc0: debugging.ParserLocation)   : Parser[Any] = "{"~repsep(member, ",")~"}"

  def arr(implicit loc0: debugging.ParserLocation)   : Parser[Any] = "["~repsep(value, ",")~"]"

  def member(implicit loc0: debugging.ParserLocation): Parser[Any] = stringLiteral~":"~value
}

import java.io.FileReader

  object ParseJSON extends JSON {
    def main(args: Array[String]) {
      val reader = new FileReader(args(0))
      println(parseAll(value, reader))
    }
  }