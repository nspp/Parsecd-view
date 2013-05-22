package parsec.gui.compiler

import javax.swing.JButton
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import parsec.gui.Client
import parsec.gui.Compiler
import parsec.gui.launcher.LaunchButton

object CompileButton extends JButton("Compile") {
  addActionListener(new ActionListener {def actionPerformed(e: ActionEvent) = {compile;LoadInViewButton.load}})
  
  def compile = {
        setEnabled(false);
        
        Client.debugViews map(_.clear)
        
        val files = Compiler.compile(Client.resourcePath)
//        Client.initClient(Compiler.getMainDebuggable(Client.resourcePath))

        setEnabled(true);
        LaunchButton.setEnabled(true)
        Client.debugViews map(_.revalidate())
  }
}

