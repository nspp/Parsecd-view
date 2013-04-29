package parsec.gui.grammar

import javax.swing.JTextArea
import java.io.FileReader
import java.io.BufferedReader
import parsec.gui.parsingTree.ParsingStatus._
import javax.swing.text.DefaultHighlighter
import scala.util.parsing.combinator.debugging.ParserLocation

class GrammarFile(filename: String, index: Int) extends JTextArea {
  val fileName = filename
  val indexInGrammar = index
  
  def load(): Unit = {
    var inputFile = new FileReader(filename);
    var reader = new BufferedReader(inputFile);
    read(reader, null)
    reader.close()
    inputFile.close()
  }

  def highlight(name: String, loc: ParserLocation, status: ParsingStatus): Unit = {
    getHighlighter().removeAllHighlights()
    var start =getLineStartOffset(loc.line-1)+loc.column-1
    var end = start + name.length()
    if (getText(start,1)== "\"") end += 2
    getHighlighter().addHighlight(start, end, DefaultHighlighter.DefaultPainter)
    setCaretPosition(start);
  }
}