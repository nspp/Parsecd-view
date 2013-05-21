package parsec.gui

import scala.util.parsing.combinator.debugging.DebugableParsers

object Launcher {
  var parser: DebugableParsers = null
  var toRun:java.lang.reflect.Method = null

  def setRunFromName(name: String) = {
    val methods = parser.getClass().getMethods()
    toRun = parser.getClass().getMethod(name)
  }
  
  def run = {
    val op = new Thread() {
      override def run() {
        try {
          toRun.invoke(parser)
        }
        catch { case e => e.getCause().printStackTrace(); }
      } 
    }
    op.start()
  }
}