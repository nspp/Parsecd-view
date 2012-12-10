package parsec.gui

import java.util.Collections
import scala.collection.JavaConversions.SeqWrapper
import javax.swing.tree.TreeNode
import javax.swing.tree.DefaultTreeModel

object ParsingStatus extends Enumeration {
  type ParsingStatus = Value
  val UNKNOWN, SUCCESS, FAILURE = Value
}

import ParsingStatus._

object ParsingNodeIdGenerator{
  var uid = 0
  def id = {
    uid = uid +1
    uid
  }
}

sealed abstract class ParsingNode(model : DefaultTreeModel) extends TreeNode {

//  var uid = ParsingNodeIdGenerator.id
  var status = UNKNOWN
  var parent: ParsingNode = null
  var child:List[ParsingNode] = Nil;
  
  def parse(newStatus: ParsingStatus, msg: String) = {
    status = newStatus
  } 
  
  def append(elem: ParsingNode): ParsingNode = {
    child = elem::child
    elem.parent = this
    model.nodesWereInserted(this, Array[Int](child.length-1))
    elem
  }

  def getParent(): TreeNode = { parent }
  
  def level: Int = if (parent==null) 0 else 1+parent.level

  def getIndex(node: TreeNode): Int = {
    def find(acc: Int, list: List[ParsingNode], n: ParsingNode): Int = list match {
      case Nil => -1
      case h::t if n == h => acc
      case _::t => find(acc-1,t,n)
    }
//    var res = if (child.contains(node)) {child.length-child.indexOf(node)-1} else {-1}
    if (node.isInstanceOf[ParsingNode])
      find(child.length -1, child, node.asInstanceOf[ParsingNode])
    else
      -1
  }

  def getAllowsChildren(): Boolean = { true }
  def isLeaf(): Boolean = { false }
  def getChildAt(childIndex: Int): TreeNode = {
    child.dropRight(childIndex).last
  }

  def getChildCount(): Int = { child.length }
  def children(): java.util.Enumeration[ParsingNode] = { 
    java.util.Collections.enumeration(SeqWrapper(child.reverse))
  }
  
}

case class Rule(uid: Int = ParsingNodeIdGenerator.id)(name: String, model: DefaultTreeModel) extends ParsingNode(model) {
  override def toString = {
    name+uid//+"\n"+" "*level+"("+(child mkString ("\n"+" "*(level+1)))+")"
  }
}

case class Alternative(uid: Int = ParsingNodeIdGenerator.id)(model: DefaultTreeModel) extends ParsingNode(model) {
  override def toString = {
    "Alt"+uid//+
    //"\n"+" "*level+"("+(child mkString ("\n"+" "*(level+1)))+")"
  }
}

case class Sequence(uid: Int = ParsingNodeIdGenerator.id)(model: DefaultTreeModel) extends ParsingNode(model) {
  override def toString = {
    "Seq"+uid//+
    //"\n"+" "*level+"("+(child mkString ("\n"+" "*(level+1)))+")"
  }
}

case class Token(word: String, uid: Int = ParsingNodeIdGenerator.id)(model: DefaultTreeModel) extends ParsingNode(model) {
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
  
  override def toString = word+uid
}
