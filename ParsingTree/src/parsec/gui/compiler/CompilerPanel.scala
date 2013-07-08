package parsec.gui.compiler

import javax.swing.JToolBar
import javax.swing.JPanel
import java.awt.BorderLayout
import javax.swing.ButtonGroup
import java.awt.FlowLayout

object CompilerPanel extends JPanel(new BorderLayout) {
  val secondLine = new JPanel(new FlowLayout(FlowLayout.LEFT))
  secondLine.add(ChangeResourcesLocationButton)
  secondLine.add(LoadInViewButton)
  secondLine.add(CompileButton)
  
  add(ResourcesLocation, BorderLayout.NORTH)
  add(secondLine, BorderLayout.SOUTH)
}