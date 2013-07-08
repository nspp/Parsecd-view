package parsec.gui.tokens

import javax.swing.tree.DefaultTreeModel
import javax.swing.JLabel
import javax.swing.JTree
import javax.swing.JToolBar
import javax.swing.JScrollPane
import javax.swing.JPanel
import java.awt.BorderLayout
import javax.swing.tree.TreePath
import javax.swing.tree.DefaultTreeCellRenderer
import java.awt.Component
import java.awt.Color
import javax.swing.ToolTipManager
import javax.swing.tree.TreeModel
import javax.swing.JList
import javax.swing.DefaultListModel
import javax.swing.ListCellRenderer
import javax.swing.DefaultListCellRenderer
import parsec.gui.DebugView
import parsec.gui.SwingButtonControl
import parsec.gui.DebugControl
import TokenStatus._

class TokensView extends JPanel(new BorderLayout) with DebugView with TokenBuilderListener {
  val control = new SwingButtonControl
  def control_=(nControl: DebugControl) = ()
  var builder: TokenBuilder = null
  private var words: DefaultListModel = null
  private[this] var list: JList = null
  var model: DefaultTreeModel = null
  
  var currentIndex = -1
  build

  private[this] def build = {
    words = new DefaultListModel
    list = new JList(words)
    list.setCellRenderer(new TokenRenderer);
    ToolTipManager.sharedInstance().registerComponent(list);
    builder = new TokenBuilder(control, model)
    builder.addListener(this)
    add(new JLabel("Tokens"), BorderLayout.NORTH)
    add(list)
  }
  
  def clear = {
    words.clear()
    currentIndex = -1
  }
  
  def append(token: Tokens) = words.addElement(token)
  
  def write(token: Tokens, index: Int){
    if(index > words.size()) append(token)
  }
  
  def stepForward(name:String): Tokens= {
    currentIndex+=1
    var t = new Tokens(name, PARSED)
    if(currentIndex >= words.size()){
      append(t)
    }
    else{
      words.setElementAt(t,currentIndex)
    }
    t
  }
  
  def stepBack()= {
    if(currentIndex >= 0){
      (words.elementAt(currentIndex) match {case tok:Tokens => tok case _ => new Tokens("")}).status = UNKNOWN
      currentIndex -= 1
    }
  }
}

 class TokenRenderer extends DefaultListCellRenderer {
   override def getListCellRendererComponent(
       list: JList,
       value: Object,
       index: Int,
       isSelected: Boolean,
       cellHasFocus: Boolean): Component = {
     super.getListCellRendererComponent(list,value,index,isSelected,cellHasFocus)
     value match{
       case t: Tokens => t.status match{
         case PARSED => setForeground(Color.GREEN)
         case PENDING => setForeground(Color.ORANGE)
         case UNKNOWN => setForeground(Color.BLACK)
       }
       case _ =>
     }
     return this
   }
}
