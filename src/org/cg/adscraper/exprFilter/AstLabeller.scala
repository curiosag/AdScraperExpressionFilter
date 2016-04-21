package org.cg.adscraper.exprFilter

trait Labelled[T] {
  protected val label: T
  def getLabel() = label
}

class LNonTerminal[T](override protected val label: T, t: Token, children: List[AstNode with Labelled[Int]]) extends AstNonTerminal(t, children) with Labelled[T] {
  def labelledChildren = children
}
class LTerminal[T](override protected val label: T, symbol: Token) extends AstTerminal(symbol) with Labelled[T]

class AstLabeller(val node: AstNode) {

  def label(node: AstNode): AstNode with Labelled[Int] = label(-1, node)
  
  private def label(max: Int, node: AstNode): AstNode with Labelled[Int] = {
    val curr = max + 1;

    node match {
      case AstTerminal(t) => new LTerminal(curr, t)
      case AstNonTerminal(t, children) => new LNonTerminal(curr, t, iter(curr, children))
      case AstStructuralNonTerminal(name, children) => label(max, AstNonTerminal(Id(name), children))
    }
  }

  private def getMax(node: AstNode with Labelled[Int]): Int =
    {
      if (node.isInstanceOf[LTerminal[Int]]) {
        node.getLabel()
      } else {
        getMax(node.asInstanceOf[LNonTerminal[Int]].labelledChildren.last)
      }
    }

  private def iter(current: Int, l: List[AstNode]): List[AstNode with Labelled[Int]] = {
    l match {
      case (h :: t) => {
        val fst = label(current, h)
        fst :: iter(getMax(fst), t)
      }
      case List() => List()
    }
  }

  def get(): String = getLables_(label(-1, node))

  private def getLables_(node: AstNode with Labelled[Int]): String =
    {
      val c = if (node.isInstanceOf[LNonTerminal[Int]]) {
        "(" + node.asInstanceOf[LNonTerminal[Int]].labelledChildren.map(x => getLables_(x)).mkString(", ") + ")"
      } else { "" }

      node.getLabel().toString() + "-" + token(node).token + c
    }

  private def token(node: AstNode) = {
    {
      node match {
        case AstTerminal(v) => v
        case AstNonTerminal(v, children) => v
        case AstStructuralNonTerminal(name, children) => Id(name)
      }
    }
  }

}

object AstLabeller {
  def apply(node: AstNode) = new AstLabeller(node)
}