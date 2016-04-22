package org.cg.adscraper.exprFilter

abstract class Labelled {
  val label: Int
  val token: Token
}

case class LNonTerminal(override val label: Int, override val token: Token, children: List[Labelled]) extends Labelled
case class LTerminal(override val label: Int, override val token: Token) extends Labelled

class TokenLabeller(val node: AstNode) {

  def label(): Labelled = label(-1, node)

  private def label(max: Int, node: AstNode): Labelled = {
    val curr = max + 1;

    node match {
      case AstTerminal(t) => new LTerminal(curr, t)
      case AstNonTerminal(t, children) => new LNonTerminal(curr, t, iter(curr, children))
      case AstStructuralNonTerminal(name, children) => label(max, AstNonTerminal(Id(name), children))
    }
  }

  private def getMax(node: Labelled): Int =
    {
      node match {
        case LTerminal(label, _) => label
        case LNonTerminal(_, _, children) => getMax(children.last)
      }
    }

  private def iter(current: Int, l: List[AstNode]): List[Labelled] = {
    l match {
      case (h :: t) => {
        val fst = label(current, h)
        fst :: iter(getMax(fst), t)
      }
      case List() => List()
    }
  }

  def get(): String = getLables_(label(-1, node))

  private def getLables_(node: Labelled): String =
    {
      val c = node match 
      {
        case LNonTerminal(_, _, children) => "(" + children.map(x => getLables_(x)).mkString(", ") + ")"
        case _ => ""
      }
      
      node.label.toString() + "-" + node.token.value() + c
    }

}

object TokenLabeller 
{
  def apply(node: AstNode) = new TokenLabeller(node)
}
