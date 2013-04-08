package parsec.gui

import scala.util.parsing.combinator.debugging._
import javax.swing.tree.DefaultTreeModel
import parsec.gui.TokenStatus._
import scala.collection.mutable.Queue
import scala.collection.mutable.LinkedList

trait TokenBuilderListener {
  def stepForward(name: String): Tokens
  def stepBack()
}

class TokenBuilder(control: DebugControl, model: DefaultTreeModel) extends Listener {
  private[this] var cur : ParserLocation = null
  private[this] var listeners: List[TokenBuilderListener] = Nil
  var currentIndex = 0
  var size = 0
  var before: List[Tokens] = Nil
  //var after: LinkedList[Tokens] = new LinkedList[Tokens]
  var parsedCount: List[Int] = Nil
  var currentlyParsingToken:Queue[String] = new Queue[String]
  var recentlyParsedCount: Int = 0
  
  def addListener(prbl: TokenBuilderListener) = listeners = prbl::listeners

  def clear() = {
  }
  
  def stepIn(id: Int, name: String, loc: ParserLocation): Option[Notification] = {
    println("+++++++++++++++++stepIN+ before:"+before+ " cpt:"+currentlyParsingToken+" name:"+name+" rpc:"+recentlyParsedCount+" pc:"+parsedCount)
    Utils.toParserKind(name, loc) match {
      case WordParser(w,_) => {
        //if (after.isEmpty) {
       //   var t = new Tokens(w)
       //   size+=1
       //   after = after:+t
       // }
        currentlyParsingToken.enqueue(w)
      }
      case _ => {
        parsedCount = recentlyParsedCount::parsedCount
        recentlyParsedCount = 0
      }
    }
    None
  }
  
  def stepOut(id: Int, success: Boolean, msg: String): Option[Notification] = {
    println("+++++++++++++++++stepOUT+ before:"+before+ " cpt:"+currentlyParsingToken+" sucess:"+success+" rpc:"+recentlyParsedCount+" pc:"+parsedCount)
    if (!currentlyParsingToken.isEmpty){
      var name = currentlyParsingToken.dequeue()
      if(success){
        listeners.map(_ stepForward(name))
        parsedCount = (parsedCount.head+1)::parsedCount.tail
      }
    }
    else{
      if(!success){
        println("%%%%%stepback of "+parsedCount.head)
        for (i <- 1 to parsedCount.head) listeners.map(_ stepBack())
      }
      else{
        recentlyParsedCount += parsedCount.head
      }
      parsedCount = parsedCount.tail
    }
    None
  }
}