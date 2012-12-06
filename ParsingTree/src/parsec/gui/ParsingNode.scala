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
  
  def level: Int = if (parent==null) 0 else 1+parent.level

  def getIndex(node: TreeNode): Int = { 
    println("searching "+node+" in {"+(child mkString ", ")+"}")
    if (child.contains(node)) {child.length-child.indexOf(node)} else {-1} }

  def getAllowsChildren(): Boolean = { true }
  def isLeaf(): Boolean = { false }
  def getChildAt(childIndex: Int): TreeNode = { child.dropRight(childIndex-1).last }

  def getChildCount(): Int = { child.length }
  def children(): java.util.Enumeration[ParsingNode] = { 
    java.util.Collections.enumeration(SeqWrapper(child.reverse))
  }
  
  
//  override def equals (other: Any): Boolean= other.isInstanceOf[ParsingNode] && parent == other.asInstanceOf[ParsingNode].parent && child == other.asInstanceOf[ParsingNode].child
}

case class Rule(name: String) extends ParsingNode {
  override def toString = {
    name//+"\n"+" "*level+"("+(child mkString ("\n"+" "*(level+1)))+")"
  }
}
case class Alternative extends ParsingNode {
  override def toString = {
    "Alt"//+"\n"+" "*level+"("+(child mkString ("\n"+" "*(level+1)))+")"
  }
}
case class Sequence extends ParsingNode {
  override def toString = {
    "Seq"//+"\n"+" "*level+"("+(child mkString ("\n"+" "*(level+1)))+")"
  }
}

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
  
  override def toString = word
}
