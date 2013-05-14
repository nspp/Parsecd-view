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
  
    /*
     * Create the different views
     */
    var parseTreeView = new ParsingTreeView
    //var ruleDisplay = new RuleDiscovererView
    var tokensDisplay = new TokensView
    var grammarView = new GrammarView
    debugViews = parseTreeView::debugViews
    //debugViews = grammarView::tokensDisplay::parseTreeView::debugViews
    //debugViews = ruleDisplay::parseTreeView::debugViews
    /*
     * 
     */
  
  // This is the function that sets up the gui
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
        grammarView.loadGrammar(resourcePath)
        Client.initClient(Compiler.getMainDebuggable(resourcePath))

        compileButton.setEnabled(true);
        debugViews map(_.revalidate())
      }
    })

    /*
     * Organise the views
     */
    
    toolbar.add(compileButton)
    var steByStep = new StepByStepControllerView
    toolbar.add(steByStep)
    debugViews = steByStep::debugViews
    var rootSplit = new JSplitPane
    rootSplit.setDividerLocation(200)
    content.add(rootSplit)
    
        
    rootSplit.setLeftComponent(parseTreeView)
    //rootSplit.setRightComponent(ruleDisplay)
    rootSplit.setRightComponent(grammarView)
    
    content.add(toolbar, BorderLayout.NORTH)
    
    //var secondSplit = new JSplitPane
    //rootSplit.setDividerLocation(200)
   // secondSplit.setLeftComponent(rootSplit)
    //secondSplit.setRightComponent(grammarView)
    
    //content.add(secondSplit)
    /*
     * 
     */
    
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
    //parser.clearListeners()
    
    if(firstCompile){
      debugViews map(v => parser.addListener(v.builder))
      parseTreeView.builder.addListener(grammarView)
      parseTreeView.builder.setGrammarView(grammarView)
      parseTreeView.setGrammarView(grammarView)
      firstCompile = false
    }
    
    val methHandler = getMethodToRun(parser)
    val op = new Thread() {
      override def run() {
        try {
          methHandler.invoke(parser)
        }
        catch { case e => e.getCause().printStackTrace(); }
      } 
    }
    op.start()
  }
  
  def getMethodToRun(parser: DebugableParsers): java.lang.reflect.Method = {
    val methods = parser.getClass().getMethods()
    //val parsecdMethods = methods.filter(m => m.getAnnotation(classOf[ParsecDebug]) == null)
//    for(m <- methods){
//      println(m.getName()+": "+m.getAnnotations())
//    }
    //val methHandler = parsecdMethods.head
//      for(m <- parser.getClass().getMethods()){
//        println(m+": "+m.getAnnotations())
//      }
    parser.getClass().getMethod("runMain")
  }
}