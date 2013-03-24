package parsec.gui

import java.util.Collections
import scala.collection.JavaConversions.SeqWrapper
import javax.swing.tree.TreeNode
import javax.swing.tree.DefaultTreeModel

import javax.swing.tree.TreePath

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

sealed abstract class ParsingNode(model : DefaultTreeModel) extends TreeNode {

  var status = UNKNOWN
  var parent: ParsingNode = null
  var elems:List[ParsingNode] = Nil
  private[this] var length = 0;
  var reason: String = null

  def parse(newStatus: ParsingStatus, msg: String) = {
    status = newStatus
    status match {
      case FAILURE => reason = msg
      case _ => ()
    }
  }

  def append(elem: ParsingNode): ParsingNode = {
    elems = elem::elems
    elem.parent = this
    model.nodesWereInserted(this, Array[Int](length))
    length = length +1
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

case class Rule(uid: Int = ParsingNodeIdGenerator.id)(name: String, model: DefaultTreeModel) extends ParsingNode(model) {
  override def toString = name
}

case class Alternative(uid: Int = ParsingNodeIdGenerator.id)(model: DefaultTreeModel) extends ParsingNode(model) {
  override def toString = "Alt"
}

case class Sequence(uid: Int = ParsingNodeIdGenerator.id)(model: DefaultTreeModel) extends ParsingNode(model) {
  override def toString = "Seq"
}

case class Repetition(name: String, uid: Int = ParsingNodeIdGenerator.id)(model: DefaultTreeModel) extends ParsingNode(model) {
  override def toString = "Rep" + name.substring(3)
}

case class Token(word: String, uid: Int = ParsingNodeIdGenerator.id)(model: DefaultTreeModel) extends ParsingNode(model) {
  import java.util.Enumeration

  override def append(elem: ParsingNode): ParsingNode = this
  override def getAllowsChildren(): Boolean = false
  override def isLeaf(): Boolean = true
  override def toString = "Tok:"+word
}
