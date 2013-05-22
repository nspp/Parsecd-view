package com.github.hubertp.parserexperiments

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

import scala.util.parsing.combinator.debugging
import debugging.ParserMacros._  


object DebugableGrammar extends DebugableTest {

  def main(args: Array[String]) {
    //val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    val tokens = new lexical.Scanner("Tip Top Tip Top Mip Blip end")
    val mainParser = phrase(Term)
    mainParser(tokens) match {
      case Success(trees, _) =>
        try {
          println("Parsed as: " + trees)
        } catch {
          case tperror => println(tperror.toString)
        }
      case e =>

        println(e)
    }
  }
}

trait DebugableTest extends StandardTokenParsers with debugging.DebugableParsers {

  
  def runMain() : Unit = {
    DebugableGrammar.main(Array(""))
  }
  
  def runMain2() : Unit = {
    DebugableGrammar.main(Array(""))
  }


  lexical.delimiters ++= List("(", ")", "{", "}", ",", "*", "+")
  lexical.reserved   ++= List("Mip", "Mup", "Map", "Mop", "Tip", "Top", "Blip", "Blop", "Blap", "Blup", "end")
  
  def Term(implicit loc0: debugging.ParserLocation): Parser[Term] = (
    p1 ~ "end" ^^^ True
    | p2 ~ "end" ^^^ True
    | p3 ~ "end" ^^^ True
    | p4 ~ "end" ^^^ True
    | p5 ~ "end" ^^^ True
    | p6 ~ "end" ^^^ True
  )

  def p1(implicit loc0: debugging.ParserLocation) : Parser[Term] = (
      rep("Mip") ^^^ True
  )
  
  def p2(implicit loc0: debugging.ParserLocation) : Parser[Term] = (
      rep1("Map","Mop") ^^^ True
  )
  
  def p3(implicit loc0: debugging.ParserLocation) : Parser[Term] = (
      rep1("Mup") ^^^ True
  )
  
  def p4(implicit loc0: debugging.ParserLocation) : Parser[Term] = (
      rep1sep("Tip", "Top") ^^^ True
  )
  
  def p5(implicit loc0: debugging.ParserLocation) : Parser[Term] = (
      repN(3, "Blip") ^^^ True
  )
  
  def p6(implicit loc0: debugging.ParserLocation) : Parser[Term] = (
      repsep("Blop", "Blap") ^^^ True
  )
}
