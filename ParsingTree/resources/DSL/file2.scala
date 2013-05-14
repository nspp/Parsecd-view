import scala.util.parsing.combinator.syntactical._


package org.dg.biz

object OrderDSL extends StandardTokenParsers  with debugging.DebugableParsers {
def runMain() : Unit = {
  doMatch()
}
  def doMatch() {
  val dsl =
    "(buy 100 IBM shares at max 45, sell 40 Sun shares at min 24,buy 25 CISCO shares at max 56) for trading account \"A1234\""

  instr(new lexical.Scanner(dsl)) match {
    case Success(ord, _) => processOrder(ord) // ord is a ClientOrder
    case Failure(msg, _) => println(msg)
    case Error(msg, _) => println(msg)
  }
}

  def scala2JavaList(sl: List[LineItem]): java.util.List[LineItem] = {
    var jl = new java.util.ArrayList[LineItem]()
    sl.foreach(jl.add(_))
    jl
  }

  lexical.delimiters ++= List("(", ")", ",")
  lexical.reserved += ("buy", "sell", "shares", "at", "max", "min", "for", "trading", "account")

  def instr(implicit loc0: debugging.ParserLocation): Parser[ClientOrder] =
    trans ~ account_spec ^^ { case t ~ a => new ClientOrder(scala2JavaList(t), a) }

  def trans(implicit loc0: debugging.ParserLocation): Parser[List[LineItem]] =
    "(" ~> repsep(trans_spec, ",") <~ ")" ^^ { (ts: List[LineItem]) => ts }

  def trans_spec(implicit loc0: debugging.ParserLocation): Parser[LineItem] =
    buy_sell ~ buy_sell_instr ^^ { case bs ~ bsi => new LineItem(bsi._1._2, bsi._1._1, bs, bsi._2) }

  def account_spec(implicit loc0: debugging.ParserLocation) =
    "for" ~> "trading" ~> "account" ~> stringLit ^^ {case s => s}

  def buy_sell(implicit loc0: debugging.ParserLocation): Parser[ClientOrder.BuySell] =
    ("buy" | "sell") ^^ { case "buy" => ClientOrder.BuySell.BUY
                          case "sell" => ClientOrder.BuySell.SELL }

  def buy_sell_instr(implicit loc0: debugging.ParserLocation): Parser[((Int, String), Int)] =
    security_spec ~ price_spec ^^ { case s ~ p => (s, p) }

  def security_spec(implicit loc0: debugging.ParserLocation): Parser[(Int, String)] =
    numericLit ~ ident ~ "shares" ^^ { case n ~ a ~ "shares" => (n.toInt, a) }

  def price_spec(implicit loc0: debugging.ParserLocation): Parser[Int] =
    "at" ~ ("min" | "max") ~ numericLit ^^ { case "at" ~ s ~ n => n.toInt }
}