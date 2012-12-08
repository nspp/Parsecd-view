package parsec.gui

import scala.collection.immutable.Queue
import javax.swing.JTree
import javax.swing.tree.TreeNode
import java.util.EventListener
import scala.collection.immutable.List

trait GrammarListener {
  def update
}

abstract trait GrammarObject {
  var elems: Queue[GrammarObject] = Queue.empty
  var parent: GrammarObject = null
  
  def append(rule: GrammarObject) = {
    elems=elems:+rule
    update
    rule.parent = this
  }
  
  def update:Unit = parent.update
  
}

case class GrammarRule(name: String) extends GrammarObject {
  private var listeners: List[GrammarListener] = Nil

  override def update = {
    println("updating: "+this.toString)
    listeners.map(_.update)
  }

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
  var length = 2
  override def toString = parent match {
    case GrammarSequence() => "("+(completeToLength("", 0, elems.toList).substring(3))+")"
    case GrammarRule(_) => completeToLength("", 0, elems.toList).substring(3)
    case _ => "" // TODO error
  }

  def completeToLength(acc: String, idx: Int, list: List[GrammarObject]): String = list match {
    case h::t => completeToLength(acc+" | "+h, idx+1, t)
    case Nil if idx<length => completeToLength(acc+" | UNK", idx+1, Nil)
    case _ => acc
  }
}

case class GrammarSequence extends GrammarObject {
  var uid = Generator.id
  var length = 2
  override def toString = parent match {
    case GrammarAlternative() => "("+(completeToLength("", 0, elems.toList).substring(1))+")"
    case GrammarRule(_) => completeToLength("", 0, elems.toList).substring(1)
    case _ => "" // TODO error
  }
  
  def completeToLength(acc: String, idx: Int, list: List[GrammarObject]): String = list match {
    case h::t => completeToLength(acc+" "+h, idx+1, t)
    case Nil if idx<length => completeToLength(acc+" UNK", idx+1, Nil)
    case _ => acc
  }
}
