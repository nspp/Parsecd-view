package parsec.gui

import scala.util.parsing.combinator.debugging._
import java.util.ArrayList
import javax.swing.JButton
import javax.swing.tree.DefaultTreeModel
import parsec.gui.parsingTree._





class StepByStepController(control: DebugControl) extends Listener {
  var active: Boolean = false
  private[this] var stack: List[ParsingNode] = Nil
  private[this] var cur : ParserLocation = null
  
  private[this] def pause: Option[Notification] = if (active) {
    control.notification = Some(new Notification);
    control.notification;
  } else None
  
  def stepIn(id: Int, name: String, loc: ParserLocation) = {
    Utils.toParserKind(name, loc) match {
      case WordParser(w,_) => {
        stack = new Token()(w,loc,null)::stack
        pause
      }
      case OrParser(w,_) => {
        if (Utils.isInSameRule(loc, cur)) {
          (stack.head match{
            case a@Alternative(_) => stack = a::stack; None
            case a@Rule(_) => stack = a::stack; None
            case a@_ => stack= new Alternative()(w,loc,null)::stack; pause
          })
        } else {
          stack = new Rule()(loc.outerMethod,NoParserLocation, null)::stack
          cur = loc
          pause
        }
      }
      case AndParser(w,_) => {
        if (Utils.isInSameRule(loc, cur)) {
          (stack.head match{
            case a@Sequence(_) => stack = a::stack; None
            case a@_ => stack = new Sequence()(w,loc,null)::stack; pause
          })
        }else {
          cur = loc
          stack = new Sequence()(w,loc,null)::stack
          pause
        }
      }
      case RepParser(w,_) => {
          stack = new Repetition()(w,loc,null)::stack
          pause
      }
      case OtherParser(w,_) => None
      case _ => stack match {
        case h::t => stack = h::stack
        case _ => ()
      }; None
    }
  }
  
  def stepOut(id: Int, success: Boolean, msg: String) = stack match {
    case Nil => None
    case h1::h2::t if h1==h2 => stack = h2::t; None
    case _::t => stack = t; pause
  }
}