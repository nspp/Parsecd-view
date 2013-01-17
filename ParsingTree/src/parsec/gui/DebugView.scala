package parsec.gui

import javax.swing.JComponent
import javax.swing.JButton
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import scala.util.parsing.combinator.debugging.Notification
import scala.util.parsing.combinator.debugging.Listener

trait DebugControl {
  private var _notification: Option[Notification] = None
  def notification: Option[Notification] = _notification
  def notification_=(n: Option[Notification]) = _notification = n 
  def step() = notification match {
    case Some(n) => n.setReady();
    case _ => ()
  }
}

trait MetaDebugControl {
  var controls: List[DebugControl] = Nil
  def step() = controls.map(_ step())
  def addControl(control: DebugControl) = controls = control::controls
}

trait DebugView extends JComponent {
  val control: DebugControl
  def builder: Listener
  def clear: Unit
}

class SwingButtonControl extends JButton("Step") with DebugControl {
  setEnabled(false)
  private var _notification: Option[Notification] = None
  override def notification: Option[Notification] = _notification
  override def notification_=(n: Option[Notification]) = {
    n match {
      case Some(n) => setEnabled(true)
      case None => setEnabled(false)
    }
    _notification = n 
  }
  
  addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) {
        setEnabled(false)
        step()
      }
  })
}

class SwingButtonMetaControl extends JButton("Step (Global)") with MetaDebugControl {
  addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      step()
    }
  })
  def clear = controls = Nil
}

object NoControl extends DebugControl {
  override def step = ()
}