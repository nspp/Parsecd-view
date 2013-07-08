package parsec.gui.launcher

import javax.swing.JButton
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import parsec.gui.Client
import parsec.gui.Compiler
import parsec.gui.Launcher

object LaunchButton extends JButton("Run") {
  setEnabled(false)
  addActionListener(new ActionListener {def actionPerformed(e: ActionEvent) = run})
  
  def run = {
        setEnabled(false)
        
//        Client.initClient(Compiler.getMainDebuggable(Client.resourcePath))
        Launcher.run

//        setEnabled(true)
  }
}

