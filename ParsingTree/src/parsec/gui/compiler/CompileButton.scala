package parsec.gui.compiler

import javax.swing.JButton
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import parsec.gui.Client
import parsec.gui.Compiler

object CompileButton extends JButton("Compile") {
  addActionListener(new ActionListener {def actionPerformed(e: ActionEvent) = compile})
  
  def compile = {
        setEnabled(false);
        
        Client.debugViews map(_.clear)
        Client.initClient(Compiler.getMainDebuggable(Client.resourcePath))

        setEnabled(true);
        Client.debugViews map(_.revalidate())
  }
}

