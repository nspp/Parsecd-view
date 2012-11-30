package parsec.gui

import scala.util.parsing.combinator.debugging._
import java.util.ArrayList
import javax.swing.JButton

object Converter {
    def toParserKind(s: String, loc : ParserLocation): ParserKind = {
    def ignore : Boolean = s match {
      case s if(s.indexOf("Parser") >= 0)         => true
      case s if(s.indexOf("parser-map-") >= 0)    => true
      case s if(s != "" && s.head == '`')         => true
      case otherwise                              => false
    }
    s match {
      case "|" | "|||"                  => OrParser(s, loc)
      case "~" | "~>" | "<~" | "~!"     => AndParser(s, loc)
      case "phrase"                     => OtherParser(s, loc)
      case other if ignore              => IgnoredParser(other, loc)
      case normal                       => WordParser(normal, loc)
    }
  }
}

trait ParsingTreeBuilderController {
  def install (state: Boolean);
}

class ParsingTreeBuilder(control: ParsingTreeBuilderController) extends Listener {

  var head: ParsingNode = null
  var stack: List[ParsingNode] = Nil;
  var notif: Option[Notification] = None
  
  def clear() = {
    head = null
    stack = Nil
    notif = None
    control install true
  }
  
  def step() = {
    control install false
    notif match {
      case Some(n) => {
        println("setting the notification ready for parsing tree building")
        n.setReady
      }
      case None => ()
    }
  }
  
  def stepIn(id: Int, name: String, loc: ParserLocation, tree: AndOrZipper): Option[Notification] = {
    println("stepping in")
    notif = None
    var kind: ParserKind = Converter.toParserKind(name, loc)
    stack = (kind match {
      case WordParser(w,_) => {
          println(w)
    	  notif = Some(new Notification)
    	  control install true
    	  (stack.head append(new Token(w)))
      	}
      case OrParser(w,_) => {
          println(w)
        (stack.head match{
        case Alternative() => stack.head
        case _ => stack.head append new Alternative
      })
      }
      case AndParser(w,_) => {
          println(w)
        (stack.head match{
        case Sequence() => stack.head
        case _ => stack.head append new Sequence
      })
      }
      case OtherParser(n,_) => {
        println(n)
        if (stack!=Nil) stack.head append (new Rule(n)) else {
        head = new Rule(n)
        head
      }
      }
      case _ => {
        println("found "+kind)
        stack.head
      }
    })::stack
    notif
  }
  
  def stepOut(id: Int, success: Boolean, last: Boolean): Option[Notification] = {
    if (success)
      stack.head.parse(ParsingStatus.SUCCESS, "")
    else
      stack.head.parse(ParsingStatus.FAILURE, "")
    stack = stack.tail
    None
  }
}

trait RuleBuilderController {
  def discover(rule: GrammarRule)
}

class RuleBuilder(control: RuleBuilderController) extends Listener {
  var rules: java.util.List[GrammarRule] = new ArrayList
  var stack: List[GrammarObject] = Nil
  var focus: List[Int] = 0::Nil
  
  def clear = {
    rules = new ArrayList
    stack = Nil
    focus = 0::Nil
  }
  
  def stepIn(id: Int, name: String, loc: ParserLocation, tree: AndOrZipper): Option[Notification] = {
    var kind: ParserKind = Converter.toParserKind(name, loc)
    
    def ruleUpdate(focused: List[GrammarObject], current: GrammarObject, elem: GrammarObject) = focused match {
      case Nil => {
        current append elem
        stack = elem::stack
        focus = 0::focus
      }
      case h::t if (h==elem) => {
        stack = h::stack
        focus = 0::focus
      }
      case _ => // TODO Send error
    }
    
    kind match{
      case OtherParser(n,_) => {
        var rule = new GrammarRule(n)
        if (rules.contains(rule))
          rule = rules.get(rules.indexOf(rule))
        else {
          control discover(rule)
          rules.add(rule)
        }
        stack match {
          case t::h => ruleUpdate(stack.head.elems.drop(focus.head).toList, t, rule)
          case Nil => ()// We are at the head, so nothing to do
        }
      }
      case OrParser(_,_) => stack.head match {
        case GrammarAlternative() => ()
        case _ => ruleUpdate(stack.head.elems.drop(focus.head).toList, stack.head, new GrammarAlternative)
      }
      case AndParser(_,_) => stack.head match {
        case GrammarSequence() => ()
        case _ => ruleUpdate(stack.head.elems.drop(focus.head).toList, stack.head, new GrammarSequence)
      }
      case WordParser(n,_) => ruleUpdate(stack.head.elems.drop(focus.head).toList, stack.head, new Word(n))
      case _ => println("ohoh")
    }
    None
  }
  
  def stepOut(id: Int, success: Boolean, last: Boolean): Option[Notification] = {
    stack = stack.tail
    focus = focus.tail
    stack match {
      case GrammarAlternative()::t if (!success) => focus = (focus.head +1)::focus.tail
      case GrammarSequence()::t if (success) => focus = (focus.head +1)::focus.tail
      case _ => ()
    }    
    None
  }
}

