sealed abstract class Expr {
    def eval():Double
}

case class EConst(value:Double) extends Expr {
    def eval():Double = value
}

case class EAdd(left:Expr, right:Expr) extends Expr {
    def eval():Double = left.eval + right.eval
}

case class ESub(left:Expr, right:Expr) extends Expr {
    def eval():Double = left.eval - right.eval
}

case class EMul(left:Expr, right:Expr) extends Expr {
    def eval():Double = left.eval * right.eval
}

case class EDiv(left:Expr, right:Expr) extends Expr {
    def eval():Double = left.eval / right.eval
}

case class EUMinus(e:Expr) extends Expr {
    def eval():Double = -e.eval
}

import scala.util.parsing.combinator.lexical.StdLexical

class ExprLexical extends StdLexical {
    override def token: Parser[Token] = floatingToken | super.token

    def floatingToken: Parser[Token] =
        rep1(digit) ~ optFraction ~ optExponent ^^
            { case intPart ~ frac ~ exp => NumericLit(
                    (intPart mkString "") :: frac :: exp :: Nil mkString "")}

    def chr(c:Char) = elem("", ch => ch==c )
    def sign = chr('+') | chr('-')
    def optSign = opt(sign) ^^ {
        case None => ""
        case Some(sign) => sign
    }
    def fraction = '.' ~ rep(digit) ^^ {
        case dot ~ ff => dot :: (ff mkString "") :: Nil mkString ""
    }
    def optFraction = opt(fraction) ^^ {
        case None => ""
        case Some(fraction) => fraction
    }
    def exponent = (chr('e') | chr('E')) ~ optSign ~ rep1(digit) ^^ {
        case e ~ optSign ~ exp => e :: optSign :: (exp mkString "") :: Nil mkString ""
    }
    def optExponent = opt(exponent) ^^ {
        case None => ""
        case Some(exponent) => exponent
    }
}

import scala.util.parsing.combinator.syntactical._

import scala.util.parsing.combinator.debugging
import debugging.ParserMacros._  

object ExprParser extends StandardTokenParsers with debugging.DebugableParsers {

  def runMain() : Unit = {
    DebugableGrammar.main(Array(""))
  }
  
    override val lexical = new ExprLexical
    lexical.delimiters ++= List("+","-","*","/","(",")")

    def value(implicit loc0: debugging.ParserLocation) = numericLit ^^ { s => EConst(s.toDouble) }

    def parens(implicit loc0: debugging.ParserLocation):Parser[Expr] = "(" ~> expr <~ ")"

    def unaryMinus(implicit loc0: debugging.ParserLocation):Parser[EUMinus] = "-" ~> term ^^ { EUMinus(_) }

    def term(implicit loc0: debugging.ParserLocation) = ( value |  parens | unaryMinus )

    def binaryOp(implicit loc0: debugging.ParserLocation,level:Int):Parser[((Expr,Expr)=>Expr)] = {
        level match {
            case 1 =>
                "+" ^^^ { (a:Expr, b:Expr) => EAdd(a,b) } |
                "-" ^^^ { (a:Expr, b:Expr) => ESub(a,b) }
            case 2 =>
                "*" ^^^ { (a:Expr, b:Expr) => EMul(a,b) } |
                "/" ^^^ { (a:Expr, b:Expr) => EDiv(a,b) }
            case _ => throw new RuntimeException("bad precedence level "+level)
        }
    }
    val minPrec = 1
    val maxPrec = 2

    def binary(implicit loc0: debugging.ParserLocation,level:Int):Parser[Expr] =
        if (level>maxPrec) term
        else binary(level+1) * binaryOp(level)

    def expr(implicit loc0: debugging.ParserLocation) = ( binary(minPrec) | term )

    def parse(s:String) = {
        val tokens = new lexical.Scanner(s)
        phrase(expr)(tokens)
    }

    def apply(s:String):Expr = {
        parse(s) match {
            case Success(tree, _) => tree
            case e: NoSuccess =>
                   throw new IllegalArgumentException("Bad syntax: "+s)
        }
    }

    def test(exprstr: String) = {
        parse(exprstr) match {
            case Success(tree, _) =>
                println("Tree: "+tree)
                val v = tree.eval()
                println("Eval: "+v)
            case e: NoSuccess => Console.err.println(e)
        }
    }
    
    //A main method for testing
    def main(args: Array[String]) = test(args(0))
}               