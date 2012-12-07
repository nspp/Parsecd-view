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
import scala.tools.nsc
import scala.tools.nsc.reporters.ConsoleReporter
import scala.reflect.io.{PlainDirectory, Directory, PlainFile}
import java.io._
import scala.util.parsing.combinator.debugging.DebugableParsers
import java.awt.event.ActionListener
import javax.swing.ListModel
import javax.swing.tree.TreeCellRenderer
import scala.swing.MainFrame
import scala.swing.Component
import scala.util.parsing.combinator.debugging.Controllers

class StepController extends JButton with ParsingTreeBuilderController {
  def install(state: Boolean) = setEnabled(state)
  
  override def getText() = "Step"
}

class RuleUpdater(list: DefaultListModel) extends RuleBuilderController {
  var i = 0
  var l: List[GrammarUpdater] = Nil
  def discover(r: GrammarRule) = {
    list.add(i, r.toString + r.elems.mkString);
    i = i+1
    var upd = new GrammarUpdater(i, list, r)
    r.addListener(upd)
    l = upd::l
  }
  
  def clear = {
    l.map(_ uninstall)
    i = 0
    l = Nil
  }
}

class GrammarUpdater(index: Int, list: DefaultListModel, r: GrammarRule) extends GrammarListener {
  def uninstall = {
    r.removeListener(this)
  }
  def update {
    list.set(index, r.toString+r.elems.mkString)
  }
}

object Client extends SimpleSwingApplication with Controllers {
  val noRootParse = new Rule("No parsing yet", null)
  var model = new DefaultTreeModel(noRootParse)
  var content: JComponent = new JPanel(new BorderLayout)
  var parseTreeControl = new StepController
  var parseTree: ParsingTreeBuilder = new ParsingTreeBuilder(parseTreeControl, model)
  var rules = new DefaultListModel
  var ruleControl = new RuleUpdater(rules)
  var ruleTree = new RuleBuilder(ruleControl)
  var debuggedParser: DebugableParsers = null
  def top = {
	java.lang.System.setProperty("parser.combinators.debug", "true") // enable macro
	java.lang.System.setProperty("parsec.debug", "true")
    var parsing = new JTree(model)
    var ruleList = new JList()
    ruleList.setModel(rules)
    var toolbar = new JToolBar()
    parseTreeControl.setAction(new Action() {
      var enable: Boolean = true
      def removePropertyChangeListener(x: PropertyChangeListener) {
        
      }
      def addPropertyChangeListener(x: PropertyChangeListener) {
        
      }
      def isEnabled() = enable
      def setEnabled(b: Boolean) = {enable = b}
      def putValue(key: String, value: Any) = {}
      def getValue(key: String) = null
      def actionPerformed(act: ActionEvent) = {
        parseTree.step()
        model reload
      }
    })
    
    parseTreeControl.setEnabled(false)
    
    // TODO button compile
    var compileButton = new JButton
    compileButton.setText("Compile")
    compileButton.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) {
        compileButton.setEnabled(false);
        parseTreeControl.setEnabled(false);
        parseTree.clear
        ruleTree.clear
        Client.initClient(Compiler.getMainDebuggable)
        model setRoot(parseTree.head)
        parsing.setModel(model)
        rules.clear()
        parseTreeControl.setEnabled(true);
        compileButton.setEnabled(true);
      }
    })
    
    
    toolbar.add(compileButton)
    toolbar.add(parseTreeControl)
    var rootSplit = new JSplitPane
    rootSplit.setLeftComponent(parsing)
    rootSplit.setRightComponent(ruleList)
    rootSplit.setDividerLocation(200)
    content.add(rootSplit)
    content.add(toolbar, BorderLayout.NORTH)
    
    new MainFrame() {
      title = "Combinator Parsing"
      contents = Component.wrap(content)
      size = new java.awt.Dimension(800,400)
    }
  }
  
  def initClient(parser: DebugableParsers) = {
	//if (debuggedParser!=null) {
		//debuggedParser.removeListener(parseTree)
		//debuggedParser.removeListener(ruleTree)
	//}
		
    parser.addListener(parseTree)
    parser.addListener(ruleTree)
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
    
    
    val methHandler = parser.getClass().getMethod("runMain", classOf[Controller]) // runTest would be defined in Parsers and would add Controller argument to the list of listeners
    val op              = new Thread() {
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

object Compiler {
  
  def compile : List[String] = {

    def createCompiler(out: String): (nsc.Global, nsc.Settings) = {
      val settings = new nsc.Settings()
      val props = new java.util.Properties()
      props.load(new java.io.FileInputStream("local.properties"))
      val classPath = props.getProperty("scala.home") + "/lib/scala-library.jar"
      settings.classpath.value = classPath //System.getProperty("java.class.path")
      val jFile = new java.io.File(out)
      val directory = new Directory(jFile)
      val compDirectory = new PlainDirectory(directory)
      settings.outputDirs.setSingleOutput(compDirectory)

      val global = new nsc.Global(settings, new ConsoleReporter(settings))
      (global, settings)
    }

    def doCompile(filesToCompile : List[String], dest : String) {
      println("WILL COMPILE: " + filesToCompile.mkString(", "))
      val (comp, settings) = createCompiler(dest)
      val command = new nsc.CompilerCommand(filesToCompile, settings)
      val run = new comp.Run
      run compile command.files
    }

    // Get file handle of original file or directory
    val dir = "resources"
    val build = "build"
    val orig = new File(dir)
    var error : Option[String] = None
    var files : List[File] = Nil
    var fnames : List[String] = Nil
    var fpaths : List[String] = Nil

    // In case it's a directory, let the file array contain all the files of the directory
    if (orig.isDirectory) {
      files     = orig.listFiles.filter(f => """.*\.scala$""".r.findFirstIn(f.getName).isDefined).toList
      fnames    = files.map(f => f.getName)
      fpaths    = fnames.map(f => dir + "/" + f)
    }

    // Then compile the files
    doCompile(fpaths, build)
    return fnames
  }
  
  def findClass : Class[_] = {
    def findClass0(dir : File) : List[Class[_]] = {
      if (dir.isDirectory) {
        val classPointers = dir.listFiles.filter(f => """.*\.class$""".r.findFirstIn(f.getName).isDefined).toList
        val directories = dir.listFiles.filter(f => f.isDirectory).toList
        val classStrings = classPointers.map(c => c.getPath.split('.').head.split('/').drop(1).mkString("."))
        val classes = (for (c <- classStrings if c.last == '$') yield Class.forName(c)).filter(hasRun(_))
        return classes ++ directories.flatMap(findClass0)
      }
      else {
        println(dir + " is not a directory")
        throw new Exception(dir + " is not a directory")
      }
    }

    def hasRun(c : Class[_]) : Boolean = {
      (c.getDeclaredMethods.filter(m => m.getName == "runMain").length == 1)
    }

    println("searching class with a runMain method")
    val cs = findClass0(new File("build"))
    println(cs)
    cs match {
      case head::_   => head
      case _            => throw new Exception("No runDebug class in uploaded files")
    }
  }

  def getMainDebuggable(): DebugableParsers = {
    val props = new java.util.Properties()
    props.load(new java.io.FileInputStream("local.properties"))
    val x = props.getProperty("scala.home")

    val files = compile // Echoed out to save a bit of time
    
    // TODO Handle the case were the compilation threw errors !!!!

    println("Compile was successful")

    // Now find the class containing the main function
    val classToRun = findClass

    println("Class name: " + classToRun.getName)

    // Invoke the class we found, calling run with a newly created controller
    val f           = classToRun.getField("MODULE$")
    f.setAccessible(true)
    f.get(null).asInstanceOf[DebugableParsers]
  }
}
