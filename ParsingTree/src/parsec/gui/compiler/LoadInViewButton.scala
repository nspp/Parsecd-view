package parsec.gui.compiler

import javax.swing.JButton
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import parsec.gui.Client
import parsec.gui.Compiler

object LoadInViewButton extends JButton("Load in view") {
  addActionListener(new ActionListener {def actionPerformed(e: ActionEvent) = load})
  
  def load = {
        setEnabled(false);
        Client.grammarView.loadGrammar(Client.resourcePath)
        setEnabled(true);
  }
}

