package parsec.gui.parsingTree

import java.util.Collections
import scala.collection.JavaConversions.SeqWrapper
import javax.swing.tree.TreeNode
import javax.swing.tree.DefaultTreeModel
import javax.swing.tree.TreePath
import parsec.gui.Utils
import scala.util.parsing.combinator.debugging.ParserLocation

object ParsingStatus extends Enumeration {
  type ParsingStatus = Value
  val UNKNOWN, SUCCESS, FAILURE = Value
}

object ParsingNodeIdGenerator{
  var uid = 0
  def id = {
    uid = uid +1
    uid
  }
}


import ParsingStatus._

sealed abstract class ParsingNode(name: String, location: ParserLocation, model : DefaultTreeModel) extends TreeNode {

  def getName(): String = name
  var loc = location
  var status = UNKNOWN
  var parent: ParsingNode = null
  var elems:List[ParsingNode] = Nil
  var hiddens:List[ParsingNode] = Nil
  private[this] var length = 0;
  var reason: String = null
  
  var hideChildren = Utils.induceNonUserParser(name)
  def setHideChildren(hideChildren: Boolean = true): Unit = this.hideChildren = hideChildren
  var isHidden = false
  def setHidden(isHidden: Boolean = true): Unit = this.isHidden = isHidden
  
  def isInSamePosition(node: ParsingNode): Boolean = {
    (loc.fileName == node.loc.fileName) &&
    (loc.line == node.loc.line) &&
    (loc.column == node.loc.column) &&
    (loc.offset == node.loc.offset)
  }
  
  def parse(newStatus: ParsingStatus, msg: String) = {
    status = newStatus
    status match {
      case FAILURE => reason = msg
      case _ => ()
    }
  }

  def append(elem: ParsingNode): ParsingNode = {
////    if(hideChildren){
//      name match{
//      // rep(p) = rep1(p) | success
//      case "rep" => appendHide(elem)
//      // rep1sep(p,q) = p ~ rep(q ~> p) ^^ {case x~y => x::y}
//      case "rep1sep" => appendHide(elem) 
//      // repsep(p,q) = rep1sep(p, q) | success
//      case "repsep" => appendHide(elem)
//      case _ => appendFirstNonHidden(elem)
//      }
////    }
////    else{
////      appendFirstNonHidden(elem)
////    }
    if(isInSamePosition(elem)) appendHide(elem)
    else appendFirstNonHidden(elem)
  }
  
  private def appendFirstNonHidden(elem: ParsingNode): ParsingNode = {
      if(isHidden){
        parent.appendFirstNonHidden(elem)
      }
      else{
        appendShow(elem)
      }
  }
  
  private def appendShow(elem: ParsingNode): ParsingNode = {
    elems = elem::elems
    elem.parent = this
    model.nodesWereInserted(this, Array[Int](length))
    length = length +1
    elem
  }

  private def appendHide(elem: ParsingNode): ParsingNode = {
    hiddens = elem::hiddens
    elem.setHidden()
    elem.parent = this
    elem
  }

  def getParent(): TreeNode = parent

  def level: Int = if (parent==null) 0 else 1 + parent.level

  def getIndex(node: TreeNode): Int = {
    def find(acc: Int, list: List[ParsingNode], n: ParsingNode): Int = list match {
      case Nil => -1
      case h::_ if n == h => acc
      case _::t => find(acc - 1, t, n)
    }
    node match {
      case n: ParsingNode => find(length, elems, n)
      case _ => -1
    }
  }

  def getAllowsChildren(): Boolean = true
  def isLeaf(): Boolean = false
  def getChildAt(childIndex: Int): TreeNode = elems.dropRight(childIndex).last

  def getChildCount(): Int = length
  def children(): java.util.Enumeration[ParsingNode] = java.util.Collections.enumeration(SeqWrapper(elems.reverse))
  
  def path: TreePath = parent match {
    case null => new TreePath(this)
    case _ => parent.path.pathByAddingChild(this)
  }

}

case class Rule(uid: Int = ParsingNodeIdGenerator.id)(name: String, location: ParserLocation, model: DefaultTreeModel) extends ParsingNode(name, location,model) {
  override def toString = name
}

case class Alternative(uid: Int = ParsingNodeIdGenerator.id)(name: String = "Alt", location: ParserLocation, model: DefaultTreeModel) extends ParsingNode(name, location,model) {
  override def toString = "Alt"
}

case class Sequence(uid: Int = ParsingNodeIdGenerator.id)(name: String = "Seq", location: ParserLocation, model: DefaultTreeModel) extends ParsingNode(name, location,model) {
  override def toString = "Seq"
}

case class Repetition(uid: Int = ParsingNodeIdGenerator.id)(name: String, location: ParserLocation, model: DefaultTreeModel) extends ParsingNode(name, location,model) {
  override def toString = "R" + name.substring(1)
}

case class Token(uid: Int = ParsingNodeIdGenerator.id)(name: String, location: ParserLocation, model: DefaultTreeModel) extends ParsingNode(name, location,model) {
  import java.util.Enumeration

  override def append(elem: ParsingNode): ParsingNode = this
  override def getAllowsChildren(): Boolean = false
  override def isLeaf(): Boolean = true
  override def toString = "Tok:"+name
}

case class Hidden(name: String, location: ParserLocation, model: DefaultTreeModel) extends ParsingNode(name, location, model) {
  override def append(elem: ParsingNode): ParsingNode = parent.append(elem)
}
