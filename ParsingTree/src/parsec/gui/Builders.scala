package parsec.gui

import scala.util.parsing.combinator.debugging._
import java.util.ArrayList
import javax.swing.JButton
import javax.swing.tree.DefaultTreeModel

object Utils {
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
  def isInSameRule(loc1: ParserLocation, loc2: ParserLocation): Boolean = {
    if (loc2==null||loc1==null) return loc1==loc2
    return loc1.fileName == loc2.fileName && loc1.outerMethod == loc2.outerMethod &&
             (loc1.outer == loc2.outer||isInSameRule(loc1.outer, loc2.outer))
  }
  def print(loc: ParserLocation): String = loc match {
    case null => ""
    case _ => loc.fileName+":"+loc.outerMethod+"["+print(loc.outer)+"]"
  }
}

trait ParsingTreeBuilderListener {
  def parsed(node: ParsingNode, succ: Boolean, msg: String)
  def adding(node: ParsingNode, parent: ParsingNode)
}

class ParsingTreeBuilder(control: DebugControl, model: DefaultTreeModel) extends Listener {

  var head: ParsingNode = new Rule()("main", model)
  private[this] var stack: List[ParsingNode] = head::Nil
  private[this] var notif: Option[Notification] = None
  private[this] var cur : ParserLocation = null
  private[this] var fst = true
  private[this] var listeners: List[ParsingTreeBuilderListener] = Nil
  
  def addListener(prbl: ParsingTreeBuilderListener) = listeners = prbl::listeners

  def clear() = {
    head = new Rule()("main", model)
    stack = head::Nil
    notif = None
    fst = true;
  }

  def stepIn(id: Int, name: String, loc: ParserLocation): Option[Notification] = {
    notif = None
    if (fst) {
      notif = Some(new Notification)
      fst = false
      model setRoot head
    }
    Utils.toParserKind(name, loc) match {
      case WordParser(w,_) => {
        notif = Some(new Notification)
        var n = new Token(w)(model)
        stack = (stack.head append(n))::stack
        listeners.map(_ adding(n, stack.tail.head))
      }
      case OrParser(w,_) => {
        if (Utils.isInSameRule(loc, cur)) {
        (stack.head match{
        case a@Alternative(_) => stack = a::stack
        case a@Rule(_) => stack = a::stack
        case a@_ => {
          var n = new Alternative()(model)
          stack = (a append n)::stack
          listeners.map(_ adding(n, stack.tail.head))
        }
      })
        } else {
          var r = new Rule()(loc.outerMethod, model)
          stack = (stack.head append r)::stack
          listeners map(_ adding(r, stack.head))
          cur = loc
        }
      }
      case AndParser(w,_) => {
        if (Utils.isInSameRule(loc, cur)) {
        (stack.head match{
        case a@Sequence(_) => stack = a::stack
        case a@_ => {
          var n = new Sequence()(model)
          stack = (a append n)::stack
          listeners map(_ adding(n, stack.tail.head))
        }
      })
        }else {
          var r = new Rule()(loc.outerMethod, model)
          stack.head append r
          listeners map(_ adding(r, stack.head))
          cur = loc
          var n = new Sequence()(model)
          stack = (r append n)::stack
          listeners map(_ adding(n, stack.tail.head))
        }
      }
      case OtherParser(w,_) => println("OTHER!!!")
      case _ => stack = stack.head::stack
    }
    control.notification = notif
    notif
  }
  
  def stepOut(id: Int, success: Boolean, msg: String): Option[Notification] = {
    var nStatus = if (success) ParsingStatus.SUCCESS else ParsingStatus.FAILURE
    if (stack.length>2) {
      if (stack.head!=stack.tail.head)
          stack.head.parse(nStatus, msg)
          listeners map(_ parsed(stack.head, success, msg))
    } else {
        stack.head.parse(nStatus, msg)
        listeners map(_ parsed(stack.head, success, msg))
    }
    def consume: Unit = stack match {
      case (h:Rule)::h2::t if h!=h2 => cur = cur.outer
      case _ => ()
    }
    consume
    stack = stack.tail
    None
  }
}

trait RuleBuilderListener {
  def discover(rule: GrammarRule)
  def discover(parent: GrammarObject, child: GrammarObject)
}

class RuleBuilder extends Listener {
  var rules: java.util.List[GrammarRule] = new ArrayList
  private[this] var stack: List[GrammarObject] = Nil
  private[this] var focus: List[Int] = 0::Nil
  private[this] var cur: ParserLocation = null
  private[this] var listeners: List[RuleBuilderListener] = Nil
  
  def addListener(rbl: RuleBuilderListener) = listeners = rbl::listeners
  
  def clear = {
    rules = new ArrayList
    stack = Nil
    focus = 0::Nil
    cur = null
    listeners = Nil
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
        case _ => // TODO send error
      }
      focus = 0::focus
    }
    if (Utils.isInSameRule(loc, cur)) {
      Utils.toParserKind(name, loc) match {
        case WordParser(w,_) => updateRule(stack.head, focus.head, Word(w))
        case OrParser(w,_) => stack.head match {
          case a@GrammarAlternative() => stack = a::stack; a.length = a.length +1
          case r@GrammarRule(_) => stack = r::stack; r.length = r.length +1
          case _ => updateRule(stack.head, focus.head, new GrammarAlternative())
        }
        case AndParser(w,_) => stack.head match {
          case a@GrammarSequence() => stack = a::stack; a.length = a.length +1
          case _ => updateRule(stack.head, focus.head, new GrammarSequence())
        }
        case _ => stack = stack.head::stack
      }
      None
    } else {
      cur = loc
      var idx = rules.indexOf(GrammarRule(loc.outerMethod))
      var r = GrammarRule(loc.outerMethod)
      if (idx == -1) {
        rules.add(r)
        listeners.map(_ discover r)
      } else {
        r = rules.get(idx)
      }
      stack match {
        case h::t => {
          h append r
          listeners map (_.discover(h, r))
        }
        case _ => ()
      }
      stack = r::stack
      focus = 0::focus
      stepIn(id,name,loc)
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
        focus = (focus.head+1)::(focus.tail)
        stack = t
      }
      case _::t => {
        focus = focus.tail
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

class StepByStepController(control: DebugControl) extends Listener {
  var active: Boolean = false
  private[this] var stack: List[ParsingNode] = Nil
  private[this] var cur : ParserLocation = null
  
  private[this] def pause = {
    control.notification = Some(new Notification)
    control.notification
  }
  def stepIn(id: Int, name: String, loc: ParserLocation) =  if (active) {
    Utils.toParserKind(name, loc) match {
      case WordParser(w,_) => {
        stack = new Token(w)(null)::stack
        pause
      }
      case OrParser(w,_) => {
        if (Utils.isInSameRule(loc, cur)) {
          (stack.head match{
            case a@Alternative(_) => stack = a::stack; None
            case a@Rule(_) => stack = a::stack; None
            case a@_ => {
              stack= new Alternative()(null)::stack
              pause
            }
          })
        } else {
          var r = 
          stack = new Rule()(loc.outerMethod, null)::stack
          cur = loc
          pause
        }
      }
      case AndParser(w,_) => {
        if (Utils.isInSameRule(loc, cur)) {
          (stack.head match{
            case a@Sequence(_) => stack = a::stack; None
            case a@_ => {
              stack = new Sequence()(null)::stack; pause
            }
          })
        }else {
          cur = loc
          stack = new Sequence()(null)::stack
          pause
        }
      }
      case OtherParser(w,_) => None
      case _ => stack = stack.head::stack; None
    }
  } else None
  
  def stepOut(id: Int, success: Boolean, msg: String) = stack match {
    case Nil => None
    case h1::h2::t if h1==h2 => stack = h2::t; None
    case _::t => stack = t; pause
  }
}