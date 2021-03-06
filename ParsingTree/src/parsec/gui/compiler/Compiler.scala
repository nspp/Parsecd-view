package parsec.gui

import scala.tools.nsc
import scala.tools.nsc.reporters.ConsoleReporter
import scala.reflect.io.{PlainDirectory, Directory, PlainFile}
import java.io._
import scala.util.parsing.combinator.debugging.DebugableParsers
import scala.annotation.ClassfileAnnotation
import parsec.gui.launcher.ToRunPrompt


object Compiler {
  var done = false
  
  def compile(dir: String) : List[String] = {

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
      var dir = new File(dest)
      if (dir.exists()) dir.delete()
      val (comp, settings) = createCompiler(dest)
      val command = new nsc.CompilerCommand(filesToCompile, settings)
      val run = new comp.Run
      run compile command.files
    }

    // Get file handle of original file or directory
//    val dir = "resources"
    val build = "build"
    println(dir)
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
    } else if (orig.getName().endsWith(".scala")) {
      files = orig::Nil
      fnames = files.map(f => f.getName)
      fpaths = fnames.map(f => dir + "/" + f)
    }

    // Then compile the files
    doCompile(fpaths, build)
    done = true
    return fnames
  }
  
  def findClass : Class[_] = {
    var i = 0;
    def findClass0(dir : File) : List[Class[_]] = {
      if (dir.isDirectory) {
        val classPointers = dir.listFiles.filter(f => """.*\.class$""".r.findFirstIn(f.getName).isDefined).toList
        val directories = dir.listFiles.filter(f => f.isDirectory).toList
        val classStrings = classPointers.map(c => c.getPath.split('.').head.split(System.getProperty("file.separator").charAt(0)).drop(1).mkString("."))
        println((classStrings.filter(_.last=='$').mkString(System.getProperty("line.separator"))))
        var cl1 = (for (c <- classStrings if c.last == '$') yield Class.forName(c))
        val classes = cl1.filter(hasRun(_))
        return classes ++ directories.flatMap(findClass0)
      }
      else {
        println(dir + " is not a directory")
        throw new Exception(dir + " is not a directory")
      }
    }

    def hasRun(c : Class[_]) : Boolean = {
      (c.getDeclaredMethods.filter(m => m.getName == ToRunPrompt.getText()).length == 1)
    }
    
    def hasMethod(c : Class[_], name: String): Boolean = {
      (c.getDeclaredMethods.filter(m => m.getName == name).length == 1)
    }

    println("searching class with a runMain method")
    val cs = findClass0(new File("build"))
    println(cs)
    cs match {
      case head::_   => head
      case _            => throw new Exception("No runDebug class in uploaded files")
    }
  }
}
