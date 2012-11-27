package parsec.gui

import javax.swing.tree.TreeNode
import java.util.Collections
import scala.collection.JavaConversions.SeqWrapper

object ParsingStatus extends Enumeration {
  type ParsingStatus = Value
  val UNKNOWN, SUCCESS, FAILURE = Value
}
import ParsingStatus._

sealed abstract class ParsingNode extends TreeNode {

  var status = UNKNOWN
  var parent: ParsingNode = null
  var child:List[ParsingNode] = Nil;
  
  def parse(newStatus: ParsingStatus, msg: String) = {
    status = newStatus
  } 
  
  def append(elem: ParsingNode): ParsingNode = {
    child = elem::child
    elem.parent = this
    elem
  }

  def getParent(): TreeNode = { parent }

  def getIndex(node: TreeNode): Int = { if (child.contains(node)) {child.length-child.indexOf(node)} else {-1} }

  def getAllowsChildren(): Boolean = { true }
  def isLeaf(): Boolean = { false }
  def getChildAt(childIndex: Int): TreeNode = { child.dropRight(childIndex-1).last }

  def getChildCount(): Int = { child.length }
  // PBM : how to make an enumeration?
  def children(): java.util.Enumeration[ParsingNode] = { 
    java.util.Collections.enumeration(SeqWrapper(child.reverse))
  }
}

case class Rule(name: String) extends ParsingNode
case class Alternative extends ParsingNode
case class Sequence extends ParsingNode

case class Token(word: String) extends ParsingNode {
  import java.util.Enumeration
  
  var reason: String = null
  
  // TODO send exception actually
  override def append(elem: ParsingNode): ParsingNode = this
  
  override def parse(newStatus: ParsingStatus, msg: String) = {
    status = newStatus
    status match {
      case FAILURE => reason = msg
      case _ => ()
    }
  }
  override def isLeaf(): Boolean = { true }
  override def getAllowsChildren(): Boolean = { false }
}
