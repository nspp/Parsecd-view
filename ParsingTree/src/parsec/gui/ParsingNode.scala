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

sealed abstract class ParsingNode(model : DefaultTreeModel) extends TreeNode {

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
    println("searching "+node+" in {"+(child mkString ", ")+"}")
    if (child.contains(node)) {child.length-child.indexOf(node)} else {-1} }

  def getAllowsChildren(): Boolean = { true }
  def isLeaf(): Boolean = { false }
  def getChildAt(childIndex: Int): TreeNode = { child.dropRight(childIndex).last }

  def getChildCount(): Int = { child.length }
  def children(): java.util.Enumeration[ParsingNode] = { 
    java.util.Collections.enumeration(SeqWrapper(child.reverse))
  }
  
  
//  override def equals (other: Any): Boolean= other.isInstanceOf[ParsingNode] && parent == other.asInstanceOf[ParsingNode].parent && child == other.asInstanceOf[ParsingNode].child
}

case class Rule(name: String, model: DefaultTreeModel) extends ParsingNode(model) {
  override def toString = {
    name//+"\n"+" "*level+"("+(child mkString ("\n"+" "*(level+1)))+")"
  }
}

//~ object Alternative {
	//~ var uid = 0;
	//~ def id = {
		//~ uid = uid+1
		//~ uid
	//~ }
//~ }

case class Alternative(model: DefaultTreeModel) extends ParsingNode(model) {
  //~ var aid = Alternative.id
  override def toString = {
    "Alt"//+
    //aid+
    //"\n"+" "*level+"("+(child mkString ("\n"+" "*(level+1)))+")"
  }
}

//~ object Sequence {
	//~ var uid = 0;
	//~ def id = {
		//~ uid = uid+1
		//~ uid
	//~ }
//~ }

case class Sequence(model: DefaultTreeModel) extends ParsingNode(model) {
  //var sid = Sequence.id
  override def toString = {
    "Seq"//+
    //sid+
    //"\n"+" "*level+"("+(child mkString ("\n"+" "*(level+1)))+")"
  }
}

//~ object Token {
	//~ var uid = 0;
	//~ def id = {
		//~ uid = uid+1
		//~ uid
	//~ }
//~ }

case class Token(word: String, model: DefaultTreeModel) extends ParsingNode(model) {
  import java.util.Enumeration
  //~ var tid = Token.id
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
  
  override def toString = word//+
  //(""*tid)
}
