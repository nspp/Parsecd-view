package parsec.gui

import scala.util.parsing.combinator.debugging.ParserKind
import scala.util.parsing.combinator.debugging.WordParser
import scala.util.parsing.combinator.debugging.OrParser
import scala.util.parsing.combinator.debugging.AndParser
import scala.util.parsing.combinator.debugging.OtherParser
import scala.util.parsing.combinator.debugging.Listener
import scala.util.parsing.combinator.debugging.ParserLocation
import scala.util.parsing.combinator.debugging.AndOrZipper
import scala.util.parsing.combinator.debugging.Notification
import scala.util.parsing.combinator.debugging.OtherParser
import java.util.ArrayList
import javax.swing.JButton

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
      case Some(n) => n.setReady
      case None => ()
    }
  }
  
  def stepIn(id: Int, name: String, loc: ParserLocation, tree: AndOrZipper): Option[Notification] = {
    notif = None
    var kind: ParserKind = null
    stack = (kind match {
      case WordParser(w,_) => {
    	  notif = Some(new Notification)
    	  control install true
    	  (stack.head append(new Token(w)))
      	}
      case OrParser(_,_) => (stack.head match{
        case Alternative() => stack.head
        case _ => stack.head append new Alternative
      })
      case AndParser(_,_) => (stack.head match{
        case Sequence() => stack.head
        case _ => stack.head append new Sequence
      })
      case OtherParser(n,_) => if (stack!=Nil) stack.head append (new Rule(n)) else {
        head = new Rule(n)
        head
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
    var kind: ParserKind = null
    
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

