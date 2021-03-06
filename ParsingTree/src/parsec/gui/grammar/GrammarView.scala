package parsec.gui.grammar

import javax.swing.tree.DefaultTreeModel
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
import javax.swing.tree.MutableTreeNode
import javax.swing.tree.DefaultMutableTreeNode
import java.awt.Dimension
import javax.swing.JLabel
import parsec.gui.DebugView
import parsec.gui.NoControl
import javax.swing.JTextArea
import java.io.FileReader
import java.io.BufferedReader
import parsec.gui.grammarRules.RuleBuilder
import scala.util.parsing.combinator.debugging.ParserLocation
import javax.swing.text.DefaultHighlighter
import parsec.gui.parsingTree.ParsingStatus._
import parsec.gui.parsingTree.ParsingTreeBuilderListener
import parsec.gui.parsingTree.ParsingNode
import javax.swing.JTabbedPane
import java.io.File
import parsec.gui.parsingTree.ParsingTreeBuilderListener
import parsec.gui.parsingTree.Rule
import parsec.gui.parsingTree.ParsingNode


class GrammarView extends JPanel(new BorderLayout) with Grammar with ParsingTreeBuilderListener with DebugView{
   val control = NoControl
   var resourcePath: String = null
   var builder: GrammarNotifier = null
   var files: List[GrammarFile] = Nil
   
  var panel = new JTabbedPane()
  add(new JLabel("Grammar Files"), BorderLayout.NORTH)
  add(panel)
  add(GrammarTools, BorderLayout.SOUTH)
  build
  
  private[this] def build = {
    builder = new GrammarNotifier
    builder.addGrammar(this)
  }
   
   
  def parsed(node: ParsingNode, succ: Boolean, msg: String) = {}
  def adding(node: ParsingNode, parent: ParsingNode) = {
    node match{
      case Rule(_) => ()
      case _ => if(!node.isHidden) highlight(node.getName,node.loc, null)
    }
  }
   
  
  def loadGrammar(path: String): Unit = {
    clear
    
     resourcePath = path
     println("loading grammar from: "+path)
     /*
      * Get all the files to display
      */
    val orig = new File(resourcePath)
    var error : Option[String] = None
    var files_ : List[File] = Nil
    var fnames : List[String] = Nil
    var fpaths : List[String] = Nil

    // In case it's a directory, let the file array contain all the files of the directory
    if (orig.isDirectory) {
      files_     = orig.listFiles.filter(f => """.*\.scala$""".r.findFirstIn(f.getName).isDefined).toList
      fnames    = files_.map(f => f.getName)
      fpaths    = fnames.map(f => resourcePath + System.getProperty("file.separator") + f)
    } else if (orig.getName().endsWith(".scala")) {
      files_ = orig::Nil
      fnames = files_.map(f => f.getName)
      fpaths = fnames.map(f => resourcePath + System.getProperty("file.separator") + f)
    }
    /*
     * 
     */
    var ind = 0
    def createGrammarFile(name: String) = {
      var file = new GrammarFile(resourcePath + System.getProperty("file.separator") + name,ind)
      ind = ind +1
      file load
      var pane = new JScrollPane(file)
      panel.addTab(name, pane)
      files = file::files
    }
    fnames map createGrammarFile
    
  }
  
  def getGrammarFile(file: String): GrammarFile = {
    files.find(_.fileName == file) match {
      case Some(f) => f
      case None => {
        println("ERROR: could not find file \""+file+"\" in our loaded grammar files")
        null
      }
    }
  }
  
  def clear = {
    panel removeAll()
    files = Nil
  }
  
  def highlight(name: String, loc: ParserLocation, status: ParsingStatus): Unit = {
    val g = getGrammarFile(loc.fileName)
    if(g != null) {
      panel setSelectedIndex(g indexInGrammar)
      g highlight(name,loc,status)
    }
  }
  
  def highlight(node: ParsingNode): Unit = highlight(node.getName(),node.loc, UNKNOWN)
  
}