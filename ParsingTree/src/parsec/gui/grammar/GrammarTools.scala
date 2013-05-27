package parsec.gui.grammar

import javax.swing.JToolBar
import javax.swing.JButton
import parsec.gui.Client
import javax.swing.JTextArea
import parsec.gui.Utils
import javax.swing.text.DefaultHighlighter
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.JScrollPane
import java.awt.Color

object Tools{
  var selections: List[Any] = Nil
  
  def addskip = {
    var current = Client.grammarView.panel.getSelectedComponent().asInstanceOf[JScrollPane].getViewport().getView().asInstanceOf[GrammarFile]
    var start = current.getSelectionStart()
    var end = current.getSelectionEnd()
    println(current.getName()+" "+start+" "+end)
    selections ::= current.getHighlighter().addHighlight(start, end, new DefaultHighlighter.DefaultHighlightPainter(Color.yellow))
    Utils.addToSkip(current.fileName, start, end)
  }
  
  def removeSkip = {
    Client.grammarView.panel.getComponents().map (_.asInstanceOf[JScrollPane].getViewport().getView().asInstanceOf[GrammarFile].getHighlighter().removeAllHighlights())
    selections = Nil
    Utils.resetSkipped
  }
}

object SkipSelectionButton extends JButton("Skip selected"){
  setEnabled(true)
  addActionListener(new ActionListener {def actionPerformed(e: ActionEvent) = Tools.addskip})
  
}

object RemoveSkippedSelectionButton extends JButton("Remove skipped selections"){
  setEnabled(true)
  addActionListener(new ActionListener {def actionPerformed(e: ActionEvent) = Tools.removeSkip})
}

object GrammarTools extends JToolBar {
  add(SkipSelectionButton)
  add(RemoveSkippedSelectionButton)
}