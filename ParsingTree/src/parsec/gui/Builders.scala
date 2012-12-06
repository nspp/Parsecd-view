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

  var head: ParsingNode = new Rule("main")
  var stack: List[ParsingNode] = head::Nil;
  var notif: Option[Notification] = None
  var cur : ParserLocation = null
  var fst = true
  
  def clear() = {
    head = new Rule("main")
    stack = head::Nil
    notif = None
    control install true
  }
  
  def step() = {
    control install false
    notif match {
      case Some(n) => {
        n.setReady
      }
      case None => ()
    }
  }
  
  def stepIn(id: Int, name: String, loc: ParserLocation, tree: AndOrZipper): Option[Notification] = {
    fst = true
    def isInSameRule(loc1: ParserLocation, loc2: ParserLocation): Boolean = {
      if (loc2==null||loc1==null) return loc1==loc2
      return loc1.fileName == loc2.fileName && loc1.outerMethod == loc2.outerMethod &&
    		  (loc1.outer == loc2.outer||isInSameRule(loc1.outer, loc2.outer))
    }
    notif = None
    var kind: ParserKind = Converter.toParserKind(name, loc)
    kind match {
      case WordParser(w,_) => {
    	  notif = Some(new Notification)
    	  control install true
    	  stack = (stack.head append(new Token(w)))::stack
      	}
      case OrParser(w,_) => {
        if (isInSameRule(loc, cur)) {
        (stack.head match{
        case a@Alternative() => stack = a::stack
        case a@_ => stack= (a append new Alternative)::stack
      })
        } else {
          var r = new Rule(loc.outerMethod)
          stack.head append r
          cur = loc
          stack = (r append new Alternative)::r::stack
        }
      }
      case AndParser(w,_) => {
        if (isInSameRule(loc, cur)) {
        (stack.head match{
        case a@Sequence() => stack = a::stack
        case a@_ => stack = (a append new Sequence)::stack
      })
        }else {
          var r = new Rule(loc.outerMethod)
          stack.head append r
          cur = loc
          stack = (r append new Sequence)::r::stack
        }
      }
      case OtherParser(w,_) => ()
      case _ => stack = stack.head::stack
    }
    notif
  }
  
  def stepOut(id: Int, success: Boolean, last: Boolean): Option[Notification] = {
    if (success)
      stack.head.parse(ParsingStatus.SUCCESS, "")
    else
      stack.head.parse(ParsingStatus.FAILURE, "")
    def up() = {
      stack.head match {
		case Rule(n) => cur =cur.outer
		case _ => ()
	  }
	  stack = stack.tail
    }
    up()
    def consumeRule(): Unit = { 
      stack match {
		case (a@Rule(n))::t if (a!=head) => up(); consumeRule()
		case _ => ()
	  }
    }
    consumeRule()
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
  var cur: ParserLocation = null
  
  def clear = {
    rules = new ArrayList
    stack = Nil
    focus = 0::Nil
  }
  
  def stepIn(id: Int, name: String, loc: ParserLocation, tree: AndOrZipper): Option[Notification] = {
	def updateRule(current: GrammarObject, foc: Int, elem: GrammarObject) = {
	  current.elems.drop(foc).toList match {
	    case Nil =>  {
		  current append elem
          stack = elem::stack
		}
		case h::t if (h==elem) => stack = h::stack
        case _ => // TODO send error
	  }
  	  focus = 0::focus
	}
    def isInSameRule(loc1: ParserLocation, loc2: ParserLocation): Boolean = {
      if (loc2==null||loc1==null) return loc1==loc2
      return loc1.fileName == loc2.fileName && loc1.outerMethod == loc2.outerMethod &&
    		  (loc1.outer == loc2.outer||isInSameRule(loc1.outer, loc2.outer))
    }
    if (isInSameRule(loc, cur)) {
      var kind: ParserKind = Converter.toParserKind(name, loc)
      kind match {
        case WordParser(w,_) => updateRule(stack.head, focus.head, Word(w))
        case OrParser(w,_) => stack.head match {
			case a@GrammarAlternative() => stack = a::stack
			case _ => updateRule(stack.head, focus.head, new GrammarAlternative())
		  }
        case AndParser(w,_) => stack.head match {
			case a@GrammarSequence() => stack = a::stack
			case _ => updateRule(stack.head, focus.head, new GrammarSequence())
		  }
//        case OtherParser(w,_) => ()
        case _ => stack = stack.head::stack
      }
	} else {
      cur = loc
	  var idx = rules.indexOf(GrammarRule(loc.outerMethod))
	  var r = GrammarRule(loc.outerMethod)
	  if (idx == -1) {
		rules.add(r)
		control discover r
      } else {
        r = rules.get(idx)
      }
      stack match {
		  case h::t => h append r
		  case _ => ()
	  }
	  stack = r::stack
      focus = 0::focus
	  stepIn(id,name,loc,tree)
	}
    None
  }
  
  def stepOut(id: Int, success: Boolean, last: Boolean): Option[Notification] = {
	//focus = focus.tail
	//focus = (focus.head+1)::(focus.tail)
    def up() = {
      stack match {
		case GrammarRule(_)::Nil => ()
		case GrammarRule(_)::t => {
		  cur = cur.outer
          focus = focus.tail
          focus = (focus.head+1)::(focus.tail)
		}
		case h1::h2::t if (h1==h2) => ()
		case _ => {
          focus = focus.tail
          focus = (focus.head+1)::(focus.tail)
		}
	  }
	  stack = stack.tail
    }
    up()
    def consumeRule(): Unit = { 
      stack match {
		case (a@GrammarRule(n))::t => up(); consumeRule()
		case _ => ()
	  }
    }
    consumeRule()
    println(rules)
    println()
    println(focus mkString ", ")
    println()
    println(stack mkString ", ")
    println()
    None
  }
}
