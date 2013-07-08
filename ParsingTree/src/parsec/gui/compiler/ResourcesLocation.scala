package parsec.gui.compiler

import javax.swing.JLabel
import parsec.gui.Client

object ResourcesLocation extends JLabel(" Resources Path: "+Client.resourcePath) {
  def refresh = setText("Resources Path: "+Client.resourcePath)
}