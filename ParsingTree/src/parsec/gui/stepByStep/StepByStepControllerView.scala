package parsec.gui

import javax.swing.JRadioButton
import javax.swing.JCheckBox
import java.awt.event.ItemListener
import java.awt.event.ItemEvent

class StepByStepControllerView extends JCheckBox("Step-by-step") with DebugView {
  val control = new Object with DebugControl
  val builder = new StepByStepController(control)
  def clear = while (control.notification!=None) control.step
  addItemListener(new ItemListener {
    def itemStateChanged(e: ItemEvent) {
      builder.active = e.getStateChange()==ItemEvent.SELECTED
    }
  })
}