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

class ParsingTreeView extends JPanel(new BorderLayout) with DebugView with ParsingTreeBuilderListener {
  val noRootParse = new Rule()("No parsing yet", null)
  val control = new SwingButtonControl
  def control_=(nControl: DebugControl) = ()
  var builder: ParsingTreeBuilder = null
  private[this] var tree: JTree = null
  var toolbar = new JToolBar
  toolbar.add(control)
  build

  private[this] def build = {
    var model = new DefaultTreeModel(noRootParse)
    tree = new JTree(model)
    ToolTipManager.sharedInstance().registerComponent(tree);
    tree.setCellRenderer(new ParsingRenderer)
    builder = new ParsingTreeBuilder(control, model)
    builder.addListener(this)
    add(new JLabel("Parsing Tree"), BorderLayout.NORTH)
    add(new JScrollPane(tree))
    add(toolbar, BorderLayout.SOUTH)
  }
  
  def clear = {
    removeAll()
    ToolTipManager.sharedInstance().unregisterComponent(tree);
    build
  }
  
  def parsed(node: ParsingNode, succ: Boolean, msg: String) = {
    // Nothing to do. The coloring and tooltip are managed by the renderer
    def getExpPaths(acc: List[TreePath], nodes: List[ParsingNode]): List[TreePath] = nodes match{
      case Nil => acc
      case h::t => if (tree.isExpanded(h path)) {
        (h path)::getExpPaths(acc, h elems)
      } else {
        Nil
      }:::acc
    }
    var paths = getExpPaths(Nil, node::Nil)
    tree.getModel() match {
      case m:DefaultTreeModel => m.reload(node)
      case _ => ()
    }
    paths map(tree.expandPath(_))
  }
  
  def adding(node: ParsingNode, parent: ParsingNode) = {
    var path: TreePath = parent path;
    parent match {
      case Alternative(_) | Rule(_) => {
        if (parent.getChildCount()>1) {
          tree.collapsePath(path.pathByAddingChild(parent elems(1)))
        }
      }
      case _ => () // Do nothing
    }
    tree.expandPath(path.pathByAddingChild(node))
  }
}

class ParsingRenderer extends DefaultTreeCellRenderer {

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