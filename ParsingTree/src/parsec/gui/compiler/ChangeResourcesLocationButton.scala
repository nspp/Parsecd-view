package parsec.gui.compiler

import javax.swing.JToolBar
import javax.swing.JLabel
import javax.swing.JMenuItem
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.JFileChooser
import javax.swing.JButton
import parsec.gui.Client

object ChangeResourcesLocationButton extends JButton("Change grammar path") {
  addActionListener(new ActionListener {def actionPerformed(e: ActionEvent) = choseResourcePath})
  
  def choseResourcePath = {
      var pathChooser = new JFileChooser(Client.resourcePath)
      pathChooser.setDialogTitle("Selection of the grammar location directory")
      pathChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
      pathChooser.setMultiSelectionEnabled(false)
      
      var ret = pathChooser.showOpenDialog(null)
      if (ret==JFileChooser.APPROVE_OPTION){
        Client.resourcePath = pathChooser.getSelectedFile().getAbsolutePath()
        ResourcesLocation.refresh
        println("Grammar path set to \""+Client.resourcePath+"\"")
      }
    }
}