package parsec.gui.launcher
import javax.swing.JPanel
import java.awt.BorderLayout
import javax.swing.JToolBar
import parsec.gui.SwingButtonMetaControl
import parsec.gui.StepByStepControllerView
import parsec.gui.Client

object LauncherPanel extends JToolBar() {
  add(ToRunPrompt)
  add(LaunchButton)
  add(Client.controller)    
  add(Client.stepByStep)
}