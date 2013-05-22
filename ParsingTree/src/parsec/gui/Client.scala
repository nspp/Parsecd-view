package parsec.gui

import scala.swing.SimpleSwingApplication
import javax.swing.JPanel
import javax.swing.JSplitPane
import javax.swing.JTree
import javax.swing.JList
import javax.swing.tree.DefaultTreeModel
import javax.swing.DefaultListModel
import javax.swing.JComponent
import javax.swing.JToolBar
import javax.swing.JButton
import javax.swing.Action
import java.beans.PropertyChangeListener
import java.awt.event.ActionEvent
import java.awt.BorderLayout
import scala.util.parsing.combinator.debugging._
import java.awt.event.ActionListener
import javax.swing.ListModel
import javax.swing.tree.TreeCellRenderer
import scala.swing.MainFrame
import scala.swing.Component
import javax.swing.JScrollPane
import scala.swing.MenuBar
import javax.swing.JMenuBar
import javax.swing.JMenu
import javax.swing.JMenuItem
import java.awt.Dialog
import scala.swing.FileChooser
import javax.swing.JFileChooser
import javax.swing.JFrame
import scala.swing.Menu
import parsec.gui.parsingTree.ParsingTreeView
import parsec.gui.grammarRules.RuleDiscovererView
import parsec.gui.tokens.TokensView
import parsec.gui.grammar.GrammarView
import javax.swing.JTextField
import parsec.gui.compiler.CompilerPanel
import parsec.gui.launcher.LauncherPanel
import java.awt.FlowLayout
import javax.swing.BoxLayout


/*
 * This is the main client to be launched
 */
object Client extends SimpleSwingApplication{
  // Path to which the scala sources we want to test, default directory is "resourse"
  var resourcePath = "resources"
    
  // A list of the different views run and showed by the client
  var debugViews: List[DebugView] = Nil
  
  // Variable used to track if the views and listeners have to be built
  // The implementation of this should be changed with a cleaner way to reset the listeners
  var firstCompile= true
  
  

  var parseTreeView = new ParsingTreeView
  var grammarView = new GrammarView
  var stepByStep = new StepByStepControllerView
  var controller = new SwingButtonMetaControl
    
  debugViews = stepByStep::parseTreeView::debugViews
  
  def top = {
    var content: JComponent = new JPanel(new BorderLayout)
    java.lang.System.setProperty("parser.combinators.debug", "true") // enable macro
    java.lang.System.setProperty("parsec.debug", "true")

    var rootSplit = new JSplitPane
    rootSplit.setDividerLocation(250)
        
    rootSplit.setLeftComponent(parseTreeView)
    rootSplit.setRightComponent(grammarView)
    
    var tools = new JPanel(new BorderLayout)
    tools.add(CompilerPanel, BorderLayout.NORTH)
    tools.add(LauncherPanel)
    
    content.add(tools, BorderLayout.NORTH)
    content.add(rootSplit)
    
    
    debugViews map(v => controller.addControl(v.control))
    
    var menusBar = new MenuBar
 
    new MainFrame (){
      title = "Combinator Parsing"
      contents = Component.wrap(content)
      size = new java.awt.Dimension(750,600)
    }
  }
  
  def initClient(parser: DebugableParsers) = {
    
    // TODO Unsubscribe every listener from the ancient parser
//    parser.clearListeners()
    
    if(firstCompile){
      debugViews map(v => parser.addListener(v.builder))
      parseTreeView.builder.addListener(grammarView)
      parseTreeView.builder.setGrammarView(grammarView)
      parseTreeView.setGrammarView(grammarView)
      firstCompile = false
    }
    
//    Launcher.parser = parser
  }
}