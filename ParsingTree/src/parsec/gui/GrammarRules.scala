package parsec.gui

import scala.collection.immutable.Queue
import javax.swing.JTree
import javax.swing.tree.TreeNode
import java.util.EventListener

trait GrammarListener {
  def update
}

abstract trait GrammarObject {
  var elems: Queue[GrammarObject] = Queue.empty
  var parent: GrammarObject = null
  
  def append(rule: GrammarObject) = {
    elems=elems:+rule
    rule.parent = this
  }
  
  def update:Unit = parent.update
  
}

case class GrammarRule(name: String) extends GrammarObject {
  private var listeners: List[GrammarListener] = Nil

  override def update = listeners.map(_.update)

  def addListener(listener: GrammarListener) {
    listeners = listener::listeners
  }
  
  def removeListener(listener : GrammarListener) = listeners = listeners filterNot(_==listener)
  
  override def toString() = name
}

case class Word(name: String) extends GrammarObject {
  override def toString = name  
  // override append to throw an exception ?
}

object Generator {
	var uid = 0;
	def id = {
		uid = uid+1
		uid
	}
}

case class GrammarAlternative extends GrammarObject {
  var uid = Generator.id
  override def toString = "Alt"+uid+": "+elems.mkString("\n | ")
}

case class GrammarSequence extends GrammarObject {
  var uid = Generator.id
  override def toString = "Seq"+uid+": "+elems.mkString(" ")
}
