package parsec.gui

import scala.util.parsing.combinator.debugging._
import java.util.ArrayList
import javax.swing.JButton
import javax.swing.tree.DefaultTreeModel
import parsec.gui.launcher.SkipToEndButton
import parsec.gui.launcher.StepByStepCheckBox



object Utils {
  def toParserKind(s: String, loc : ParserLocation): ParserKind = {
    println("%%%%%"+s+" "+loc.outer+" "+loc.offset+" "+loc.line+" "+loc.column+" "+loc.fileName+" "+loc.outerMethod+" "+loc.isValid)
    def ignore : Boolean = s match {
      case s if(s.indexOf("Parser") >= 0)         => true
      case s if(s.indexOf("parser-map-") >= 0)    => true
      case s if(s != "" && s.head == '`')         => true
      case otherwise                              => false
    }
    s match {
      case "|" | "|||"                  => OrParser(s, loc)
      case "~" | "~>" | "<~" | "~!"     => AndParser(s, loc)
      case "rep" | "rep1" | "rep1sep" | "repN" | "repsep" 
      									=> RepParser(s, loc)
      case "phrase"                     => OtherParser(s, loc)
      case other if ignore              => IgnoredParser(other, loc)
      case normal                       => WordParser(normal, loc)
    }
  }
  def isInSameRule(loc1: ParserLocation, loc2: ParserLocation): Boolean = {
    if (loc2==null||loc1==null) return loc1==loc2
    return loc1.fileName == loc2.fileName && loc1.outerMethod == loc2.outerMethod &&
             (loc1.outer==loc2.outer||(loc1.outer.fileName==loc2.outer.fileName&&loc1.outer.outerMethod==loc2.outer.outerMethod))
  }
  def isInSameRuleDeep(inner: ParserLocation, outer: ParserLocation): Boolean = {
    def inside(inner: ParserLocation, outer: ParserLocation): Boolean = if (inner==null) return true
      else if (outer==null) return false
      else isInSameRuleDeep(inner,outer)||inside(inner,outer.outer)
    if (inner==null||outer==null) return inner==outer
    return inner.fileName == outer.fileName && inner.outerMethod == outer.outerMethod &&
             inside(inner.outer, outer.outer)
  }
  def print(loc: ParserLocation): String = loc match {
    case null => "End"
    case _ => loc.outerMethod+"["+print(loc.outer)+"]"
  }
  
  def induceNonUserParser(s: String): Boolean = s match{
    // rep(p) = rep1(p) | success
    case "rep" => true
    // rep1seq(p,q) = p ~ rep(q ~> p) ^^ {case x~y => x::y}
    case "rep1sep" => true
    // repsep(p,q) = rep1sep(p, q) | success
    case "repsep" => true
    case _ => false
  }
  
  def getNotification(location: ParserLocation): Option[Notification] = {
    if(! SkipToEndButton.isEnabled()) return None
    skip find (_.contains(location)) match{
      case Some(_) => {println("                           skipped "+print(location));None}
      case None => Some(new Notification)
    }
  }
  
  def getSmallStepNotification(location: ParserLocation): Option[Notification] = {
    if(StepByStepCheckBox.isSelected())
      getNotification(location)
      else
        None
  }
  
  // TODO something better to stock the skipped portions of grammar
  class Skip(file: String, start: Integer, end: Integer){
    def contains(loc: ParserLocation): Boolean = {
      println("                                      ssssssssssss "+loc.fileName+" "+file)
      if(loc.fileName != file) return false
      val pos = (Client.grammarView.getGrammarFile(file).getLineStartOffset(loc.line-1)+loc.column-1)
      (pos >= start) && (pos <= end)
    }
  }
  var skip: List[Skip] = Nil
  def addToSkip(file: String, start: Integer, end: Integer) = {
    skip = new Skip(file,start,end)::skip
  }
  def resetSkipped = {
    skip = Nil
  }
}