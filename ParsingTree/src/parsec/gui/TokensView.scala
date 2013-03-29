package parsec.gui

import javax.swing.tree.DefaultTreeModel
import javax.swing.JLabel
import javax.swing.JTree
import javax.swing.JToolBar
import javax.swing.JScrollPane
import javax.swing.JPanel
import java.awt.BorderLayout
import javax.swing.tree.TreePath
import javax.swing.tree.DefaultTreeCellRenderer
import java.awt.Component
import parsec.gui.ParsingStatus._
import java.awt.Color
import javax.swing.ToolTipManager
import javax.swing.tree.TreeModel

class TokensView extends JPanel(new BorderLayout) with DebugView with ParsingTreeBuilderListener {
  val noRootParse = new Rule()("No parsing yet", null)
  val sentence = new SentencePart(null)
  val control = new SwingButtonControl
  def control_=(nControl: DebugControl) = ()
  var builder: ParsingTreeBuilder = null
  private var tree: ParsingNode = null
  private[this] var words: JTree = null
  var model: DefaultTreeModel = null
  build

  private[this] def build = {
    model = new DefaultTreeModel(sentence)
    words = new JTree(model)
    ToolTipManager.sharedInstance().registerComponent(words);
    words.setCellRenderer(new ParsingRenderer2)
    builder = new ParsingTreeBuilder(control, model)
    builder.addListener(this)
    add(new JLabel("Tokens"), BorderLayout.NORTH)
    add(new JScrollPane(words))
  }
  
  def clear = {
    removeAll()
    ToolTipManager.sharedInstance().unregisterComponent(words);
    build
  }
  
  def adding(node: ParsingNode, parent: ParsingNode) = {
    var path: TreePath = parent path;
    tree.append(node)
    node match {
      case t@Token(_,_) => words.addSelectionPath((new TreePath(sentence)).pathByAddingChild(new WordInSentence(t)(model)))
      case _ => ()
    }
  }
}

class ParsingRenderer2 extends DefaultTreeCellRenderer {

  override def getTreeCellRendererComponent(tree: JTree,
                    value: Object, sel: Boolean, expanded: Boolean,
                    leaf: Boolean, row: Int, hasFocus: Boolean): Component = {

        super.getTreeCellRendererComponent(tree, value, sel,expanded, leaf, row,
                        hasFocus)
        value match {
          case node: ParsingNode => {
            def parentFailed(node: ParsingNode): Boolean = node.parent match {
              case null => false
              case p if p.status==FAILURE => true
              case p if p.status==UNKNOWN => false
              case p => parentFailed(p)
            }
            node.status match {
              case SUCCESS => if (parentFailed(node)) setForeground(Color.ORANGE) else setForeground(Color.GREEN)
              case FAILURE => setForeground(Color.RED)
              case UNKNOWN => 
            }
          if (node.reason != null) setToolTipText(node.reason)
          else setToolTipText(null)
          }
          case _ => 
        }
        return this;
    }
}