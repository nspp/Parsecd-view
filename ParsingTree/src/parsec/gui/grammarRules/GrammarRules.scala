package parsec.gui.grammarRules

import scala.collection.immutable.Queue
import javax.swing.JTree
import javax.swing.tree.TreeNode
import java.util.EventListener
import scala.collection.immutable.List

abstract trait GrammarObject {
  var elems: Queue[GrammarObject] = Queue.empty
  var parent: GrammarObject = null
  
  def append(rule: GrammarObject) = {
    println("appended "+rule+" to "+this)
    elems=elems:+rule
    rule.parent = this
  }
}

case class GrammarRule(name: String) extends GrammarObject {
  var length = 1;
  override def toString() = name
}

case class Word(name: String) extends GrammarObject {
  override def toString = name  
  // override append to throw an exception ?
}
object Printer {
  def print(list: List[GrammarObject], length: Int, sep:String) = {
    def completeToLength(acc: String, idx: Int, list: List[GrammarObject]): String = list match {
      case h::t => completeToLength(acc+sep+h, idx+1, t)
      case Nil if idx<length => completeToLength(acc+sep+"UNK", idx+1, Nil)
      case _ => acc
    }
    list match{
      case h::t => completeToLength(h toString, 1, t)
      case Nil => completeToLength("", 0, list);
    }
  }
}

case class GrammarAlternative extends GrammarObject {
  var length = 2
  override def toString = parent match {
    case GrammarSequence() => "("+Printer.print(elems toList, length, " | ")+")"
    case GrammarRule(_) => Printer.print(elems toList, length, " | ")
    case GrammarRepetition(_,_) => Printer.print(elems toList, length, " | ")
    case _ => "" // TODO error
  }
}

case class GrammarSequence extends GrammarObject {
  var length = 2
  override def toString = parent match {
    case GrammarAlternative() => "("+Printer.print(elems toList, length, " ")+")"
    case GrammarRule(_) => Printer.print(elems toList, length, " ")
    case GrammarRepetition(_,_) => Printer.print(elems toList, length, " ")
    case _ => "" // TODO error
  }
}

case class GrammarRepetition(name: String, userDefined: Boolean) extends GrammarObject {
  var repetedObject: GrammarObject = null
  var length = 2
  /*
  override def toString = name match{
    //case GrammarRepetition(_) => name+"("+(repetedObject==null?"UNK":repetedObject)+")"
    case "rep" => repetedObject match{
      case insideRep@GrammarRepetition("rep1",_) => name+"("+insideRep.repetedObject+")"
      case other@_ => name+"(unexpected "+repetedObject+")" // or failure instead? (not supposed to happen)
    }
    case _ => name+"("+repetedObject+")"
  }*/
  override def toString = {
    if (userDefined){
      name+"("+repetedObject+")"
    } 
    else{
      repetedObject.toString
    }
  }
  override def append(rule: GrammarObject) = {
    super.append(rule)
    if (repetedObject == null) repetedObject = rule
  }
}