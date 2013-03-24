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

object Client extends SimpleSwingApplication with Controllers {
  var resourcePath = "resources"
  var debugViews: List[DebugView] = Nil
  var debuggedParser: DebugableParsers = null	// @manu: is debuggedParser used anywhere? (does not seem to be)
  def top = {
    var content: JComponent = new JPanel(new BorderLayout)
    java.lang.System.setProperty("parser.combinators.debug", "true") // enable macro
    java.lang.System.setProperty("parsec.debug", "true")
    var toolbar = new JToolBar()
    
    var controller = new SwingButtonMetaControl
    toolbar.add(controller)
    
    var compileButton = new JButton
    compileButton.setText("Compile")
    compileButton.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) {
        compileButton.setEnabled(false);
        
        debugViews map(_.clear)
        Client.initClient(Compiler.getMainDebuggable(resourcePath))

        compileButton.setEnabled(true);
        debugViews map(_.revalidate())
      }
    })

    toolbar.add(compileButton)
    var steByStep = new StepByStepControllerView
    toolbar.add(steByStep)
    debugViews = steByStep::debugViews
    var rootSplit = new JSplitPane
    rootSplit.setDividerLocation(250)
    content.add(rootSplit)
    
    var parseTreeView = new ParsingTreeView
    var ruleDisplay = new RuleDiscovererView
    debugViews = ruleDisplay::parseTreeView::debugViews
    
    rootSplit.setLeftComponent(parseTreeView)
    rootSplit.setRightComponent(ruleDisplay)
    content.add(toolbar, BorderLayout.NORTH)
    
    debugViews map(v => controller.addControl(v.control))
    
    var menusBar = new MenuBar
    var menu = new JMenu("File")
    menusBar.contents.append(Component.wrap(menu))
    var menuItem = new JMenuItem("Choose grammar location")
    menu.add(menuItem)
    menuItem.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) {
        var pathChooser = new JFileChooser(resourcePath)
        pathChooser.setDialogTitle("Selection of the grammar location directory")
        pathChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
        pathChooser.setMultiSelectionEnabled(false)
        
        var ret = pathChooser.showOpenDialog(null)
        if (ret==JFileChooser.APPROVE_OPTION)
          resourcePath = pathChooser.getSelectedFile().getAbsolutePath()
        println(resourcePath)
      }
    })
    new MainFrame (){
      title = "Combinator Parsing"
      contents = Component.wrap(content)
      size = new java.awt.Dimension(750,600)
      menuBar = menusBar
    }
  }
  
  def initClient(parser: DebugableParsers) = {
    // TODO Unsubscribe every listener from the ancient parser
    debugViews map(v => v.clear)
    debugViews map(v => parser.addListener(v.builder))
    
//    val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
//    val op              = new Thread() {
//      override def run() {
//        try {
//          parser.runDebug(tokens)
//        }
//        catch { case e => e.getCause().printStackTrace(); }
//      } 
//    }
//    op.start()
    
    val methHandler = parser.getClass().getMethod("runMain", classOf[Controller])
    val op = new Thread() {
      override def run() {
        try {
          val controller = new Controller
          methHandler.invoke(parser, controller)
        }
        catch { case e => e.getCause().printStackTrace(); }
      } 
    }
    op.start()
  }
}