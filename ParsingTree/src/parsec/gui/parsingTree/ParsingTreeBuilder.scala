package parsec.gui.parsingTree

import scala.util.parsing.combinator.debugging._
import java.util.ArrayList
import javax.swing.JButton
import javax.swing.tree.DefaultTreeModel
import parsec.gui.DebugControl
import parsec.gui.Utils
import scala.collection.mutable.Queue


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
  private[this] var inducedParsers: Queue[String] = new Queue[String]
  
  def addListener(prbl: ParsingTreeBuilderListener) = listeners = prbl::listeners

  def clear() = {
    head = new Rule()("main", model)
    stack = head::Nil
    notif = None
    fst = true;
  }
  
  private def testInducedParser(name: String) = Utils.induceNonUserParser(name) match{
    case null => ()
    case s@_ => {println("#### met a parser "+name+", will expect following parser to be "+s);inducedParsers.enqueue(s)}
  }
  
  def stepIn(id: Int, name: String, loc: ParserLocation): Option[Notification] = {
    notif = None
    if (fst) {
      notif = Some(new Notification)
      fst = false
      model setRoot head
    }
    if(inducedParsers.isEmpty)
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
          stepIn(id,name,loc)
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
          stack = (stack.head append r)::stack
          listeners map(_ adding(r, stack.head))
          cur = loc
          stepIn(id,name,loc)
        }
      }
      
      case RepParser(w,_) => {
        if (Utils.isInSameRule(loc, cur)) {
        (stack.head match{
        case a@_ => {
          var n = new Repetition(w)(model)
          stack = (a append n)::stack
          listeners map(_ adding(n, stack.tail.head))
        }
      })
      testInducedParser(name)
        }else {
          var r = new Rule()(loc.outerMethod, model)
          stack = (stack.head append r)::stack
          listeners map(_ adding(r, stack.head))
          cur = loc
          stepIn(id,name,loc)
        }
      }
      
      case OtherParser(w,_) => ()
      case _ => stack = stack.head::stack
    }
    else{
      var s = inducedParsers.dequeue()
      var h = new Hidden(model)
      stack = (stack.head append h)::stack
      
      if(s == name){
        println("--! parser with name " + name + " not shown (not user defined)")
      }
      else
        println("--! unexepected parser "+ name + ", should be scala-defined parser " + s)
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
//      if (stack.length == 1){
        stack.head.parse(nStatus, msg)
        listeners map(_ parsed(stack.head, success, msg))
//      } else{
//        stack = null::stack
//      }
    }
    def consume: Unit = stack match {
      case (h:Rule)::h2::t if h!=h2 => cur = cur.outer; stack = stack.tail
      case _ => ()
    }
    if(! stack.isEmpty)
      stack = stack.tail
    consume
    None
  }
}