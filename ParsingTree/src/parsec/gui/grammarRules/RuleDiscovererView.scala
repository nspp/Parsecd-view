package parsec.gui.grammarRules

import javax.swing.tree.DefaultTreeModel
import javax.swing.JTree
import javax.swing.JToolBar
import javax.swing.JScrollPane
import javax.swing.JPanel
import java.awt.BorderLayout
import javax.swing.tree.TreePath
import javax.swing.tree.DefaultTreeCellRenderer
import java.awt.Component
import java.awt.Color
import javax.swing.ToolTipManager
import javax.swing.tree.MutableTreeNode
import javax.swing.tree.DefaultMutableTreeNode
import java.awt.Dimension
import javax.swing.JLabel
import parsec.gui.DebugView
import parsec.gui.NoControl

// This is a trace feature
class RuleDiscovererView extends JPanel(new BorderLayout) with RuleBuilderListener with DebugView {
  val control = NoControl
  private[this] var treeRoot = new DefaultMutableTreeNode
  private[this] var treeModel = new DefaultTreeModel(treeRoot)
  private[this] var tree = new JTree(treeRoot)
  tree.setRootVisible(false)
  tree.setShowsRootHandles(true)
  tree.setEditable(true)
  tree.setCellRenderer(new RuleDiscovererRenderer)
  private[this] var scroll = new JScrollPane(tree)
  add(new JLabel("Rule Discovered"), BorderLayout.NORTH)
  add(scroll)
  private[this] var rules: List[(GrammarRule, GrammarNode)] = Nil
  var builder: RuleBuilder = null
  build
  
  private[this] def build = {
    treeModel = new DefaultTreeModel(treeRoot)
    tree.setModel(treeModel)
    builder = new RuleBuilder
    builder.addListener(this)
  }
  
  def clear = {
    treeRoot = new DefaultMutableTreeNode
    treeModel.setRoot(treeRoot)
    rules = Nil
    builder.clear
  }
  
  def clear2 = {
    treeRoot.removeAllChildren()
    rules = Nil
    builder.clear
    //build
  }
  
  private[this] def getPath(n: GrammarRule): TreePath = (rules.filter(_._1==n).map(n => new TreePath(treeRoot).pathByAddingChild(n._2))).head
  
  def discover(rule: GrammarRule) = {
    var node = new GrammarNode(rule)
    rules = (rule, node)::rules
    treeModel.insertNodeInto(node, treeRoot, treeRoot.getChildCount())
    tree.scrollPathToVisible(new TreePath(treeRoot).pathByAddingChild(node))
    tree.setModel(treeModel)
  }
  
  def discover(parent: GrammarObject, child:GrammarObject) = parent match {
    case r:GrammarRule => rules.filter(_._1==r).map(n => {
      var node = new GrammarNode(child)
      treeModel.insertNodeInto(node, n._2, n._2.getChildCount())
      tree.makeVisible(getPath(r).pathByAddingChild(node));
    })
    case _ => {
      def parentRule(n: GrammarObject): (GrammarRule, Int) = {
        n.parent match {
          case r:GrammarRule => {
            println("r")
            (r, r.elems.indexOf(n))
          }
          case null => null
          case o@_ => {
            println("other")
            parentRule(o)
          }
        }
      }
      var p = parentRule(parent)
      var node = rules.filter(_._1==p._1).map(n => n._2.getChildAt(p._2)).head
      tree.getModel() match {
        case m:DefaultTreeModel => m.reload(node.getParent())
        case _ => ()
      }
      tree.scrollPathToVisible(getPath(p._1).pathByAddingChild(node))
    }
  }
}

object Generator {
  var cnt = -1
  def id = {
    cnt = cnt+1
    cnt
  }
}

class GrammarNode(elem: GrammarObject, uid: Int=Generator.id) extends DefaultMutableTreeNode {
  override def toString = elem.toString()
}

class RuleDiscovererRenderer extends DefaultTreeCellRenderer {

  override def getTreeCellRendererComponent(tree: JTree,
                    value: Object, sel: Boolean, expanded: Boolean,
                    leaf: Boolean, row: Int, hasFocus: Boolean): Component = {
    var c = super.getTreeCellRendererComponent(tree, value, sel,expanded, leaf, row,
                    hasFocus)
    var fm = getFontMetrics(getFont())
    c match {
      case l:JLabel => getPreferredSize().width = l.getText().foldLeft(0)((w, ch)=>w+fm.charWidth(ch))+getIconTextGap()
      case _ => ()
    }
    return this;
  }
}