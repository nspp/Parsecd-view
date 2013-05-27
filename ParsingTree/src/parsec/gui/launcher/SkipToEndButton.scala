package parsec.gui.launcher

import javax.swing.JButton
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import parsec.gui.Client
import parsec.gui.Compiler
import parsec.gui.Launcher

object SkipToEndButton extends JButton("Skip to end") {
  setEnabled(false)
  addActionListener(new ActionListener {def actionPerformed(e: ActionEvent) = endIt})
  
  def endIt = {
        setEnabled(false)
        
        Client.parseTreeView.control.step
        
//        new Thread(new Runnable {
//          def run() {
//            while(true) Client.parseTreeView.control.step()
//            }
//          }).start()
        

        //setEnabled(true)
  }
}

