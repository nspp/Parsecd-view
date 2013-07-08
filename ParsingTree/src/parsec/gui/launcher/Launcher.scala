package parsec.gui

import scala.util.parsing.combinator.debugging.DebugableParsers
import parsec.gui.launcher.ToRunPrompt
import parsec.gui.launcher.SkipToEndButton

object Launcher {
  var parser: DebugableParsers = null
  var toRun:java.lang.reflect.Method = null

  def setRunFromName(name: String) = {
    val methods = parser.getClass().getMethods()
    toRun = parser.getClass().getMethod(name)
  }
  
  def run = {
    
    val classToRun = Compiler.findClass

    println("Class name: " + classToRun.getName)

    // Invoke the class we found, calling run with a newly created controller
    val f           = classToRun.getField("MODULE$")
    f.setAccessible(true)
    parser = f.get(null).asInstanceOf[DebugableParsers]

    Client.initClient(parser)
    
    setRunFromName(ToRunPrompt.getText())
        
    val op = new Thread() {
      override def run() {
        try {
          toRun.invoke(parser)
        }
        catch { case e => e.getCause().printStackTrace(); }
      } 
    }
    SkipToEndButton.setEnabled(true)
    op.start()
  }
  
}