package parsec.gui.grammarRules

import scala.util.parsing.combinator.debugging._
import java.util.ArrayList
import javax.swing.JButton
import javax.swing.tree.DefaultTreeModel
import parsec.gui.Utils


trait RuleBuilderListener {
  def discover(rule: GrammarRule)
  def discover(parent: GrammarObject, child: GrammarObject)
}

class RuleBuilder extends Listener {
  var rules: java.util.List[GrammarRule] = new ArrayList
  private[this] var stack: List[GrammarObject] = Nil
  private[this] var focus: List[Int] = Nil
  private[this] var cur: ParserLocation = null
  private[this] var listeners: List[RuleBuilderListener] = Nil
  private[this] var lengths: List[Int] = Nil
  
  def addListener(rbl: RuleBuilderListener) = listeners = rbl::listeners
  
  def clear = {
    rules.clear
    rules = new ArrayList
    stack = Nil
    focus = Nil
    lengths = Nil
    cur = null
  }
  
  def stepIn(id: Int, name: String, loc: ParserLocation): Option[Notification] = {
    def updateRule(current: GrammarObject, foc: Int, elem: GrammarObject) = {
      current.elems.drop(foc).toList match {
        case Nil =>  {
          current append elem
          listeners map(_.discover(current, elem))
          stack = elem::stack
        }
        case h::t if (h==elem) => stack = h::stack
        case h::t => println("AOUCH!!!!!!!!!!!!!!!!!!!!!!!!! "+elem+" instead of "+h+" in "+current+" at " + foc+"\n\n"+Utils.print(cur)+"\n\n"+Utils.print(loc)); stack = elem::stack
      }
      focus = 0::focus
      elem match{
        case e:GrammarAlternative => lengths = 2::lengths;
        case e:GrammarSequence => lengths = 2::lengths;
        case e:GrammarRepetition => lengths = 2::lengths;
        case _ => lengths = 1::lengths
      }
    }
    if (Utils.isInSameRuleDeep(loc, cur)) {
      Utils.toParserKind(name, loc) match {
        case WordParser(w,_) => updateRule(stack.head, focus.head, Word(w))
        case OrParser(w,_) => stack.head match {
          case a@GrammarAlternative() => {
            stack = a::stack
            lengths = (lengths.head+1)::lengths.tail
            if (lengths.head>a.length) a.length = a.length +1
          }
          case r@GrammarRule(_) => {
            stack = r::stack
            lengths = (lengths.head+1)::lengths.tail
            if (lengths.head>r.length) r.length = r.length +1
          }
          case _ => updateRule(stack.head, focus.head, new GrammarAlternative())
        }
        case AndParser(w,_) => stack.head match {
          case a@GrammarSequence() => {
            stack = a::stack
            lengths = (lengths.head+1)::lengths.tail
            if (lengths.head>a.length) a.length = a.length +1
          }
          case _ => updateRule(stack.head, focus.head, new GrammarSequence())
        }
        case RepParser(w,_) => stack.head match {
         /* case rep@GrammarRepetition(_) => {
          }*/
          case _ =>  updateRule(stack.head, focus.head, new GrammarRepetition(w,loc.fileName != "<none>"))
        }
        case _ => stack = stack.head::stack;
      }

      None
    } else {
      Utils.toParserKind(name, loc) match {
        case WordParser(_,_) | OrParser(_,_) | AndParser(_,_) | RepParser(_,_) => {
          var idx = rules.indexOf(GrammarRule(loc.outerMethod))
          var r = GrammarRule(loc.outerMethod)
          if (idx == -1) {
            rules.add(r)
            listeners.map(_ discover r)
          } else {
            r = rules.get(idx)
          }
          stack match {
            case h::t => updateRule(h, focus.head, r)
            case _ => {
              stack = r::stack
              focus = 0::focus
              lengths = 1::lengths
            }
          }
          cur = loc
          stepIn(id,name,loc)
        }
        case _ => {
          stack match {
            case h::t => stack = h::stack
            case _ => stack = Nil
          }
          None
        }
      }
    }
  }

  def stepOut(id: Int, success: Boolean, msg: String): Option[Notification] = {
    def up() = stack match {
      case Nil => ()
      case GrammarRule(_)::Nil => stack = Nil
      case h1::h2::t if (h1==h2) => stack = h2::t
      case GrammarRule(_)::t => {
        cur = cur.outer
        focus = focus.tail
        lengths = lengths.tail
        focus = (focus.head+1)::(focus.tail)
        stack = t
      }
      case _::t => {
        focus = focus.tail
        lengths = lengths.tail
        focus = (focus.head+1)::(focus.tail)
        stack = t
      }
    }
    def consumeRule(): Unit = stack match {
      case (a@GrammarRule(n))::b::t if a!=b => up(); consumeRule()
      case _ => ()
    }
    up()
    consumeRule()
    None
  }
}