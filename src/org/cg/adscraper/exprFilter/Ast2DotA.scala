package org.cg.adscraper.exprFilter

import org.cg.scala.expressionparser._


private class AstPart(val structure: String, val labels: String) {
  def combined = "%s\n%s\n".format(structure, labels)
}

class Ast2DotA(val node: AstNode) {

  private val emptyPart = new AstPart("", "")

  def get(): String = "digraph g{\n" + get_("0", node).combined + "}"

  private def get_(prefix: String, node: AstNode): AstPart = {
    node match {
      case AstTerminal(_) => emptyPart
      case AstNonTerminal(_, children) => {
        // there is no way not to loose the generic type, also not using countedImplicit
        val counted_s = counted(0, prefix, _: List[AstNode], _: (Integer, String, AstNode) => String)
        val counted_p = counted(0, prefix, _: List[AstNode], _: (Integer, String, AstNode) => AstPart)

      
        val structure = "%s -> {%s}".format(
          digNode(prefix, node),
          counted_s(children, (i, p, n) => digNode(extendPrefix(p, i), n)).mkString("; "))
        val labels = digLabel(prefix, node) + "\n" + counted_s(children, (i, p, n) => digLabel(extendPrefix(p, i), n)).mkString("\n")

        val subs = counted_p(children, (i, p, x) => get_(extendPrefix(p, i), x))
        val subStructures = subs.map(x => x.structure).mkString("\n")
        val subLabels = subs.map(x => x.labels).mkString("\n")

        new AstPart(structure + "\n" + subStructures, labels + "\n" + subLabels)
      }
      case AstStructuralNonTerminal(name, children) => get_(prefix, AstNonTerminal(Id(name), children))
    }
  }

  private def extendPrefix(prefix: String, add: Integer) = "%s%d".format(prefix, add)
  
  private def countedImplicit[T](current: Integer) = counted(current, _:String, _: List[AstNode], _: (Integer, String, AstNode) => T)

  private def counted[T](current: Integer, prefix: String, l: List[AstNode], format: (Integer, String, AstNode) => T): List[T] = {
    l match {
      case (h :: t) => format(current, prefix, h) :: counted(current + 1, prefix, t, format)
      case List() => List()
    }
  }

  private def digNode(prefix: String, n: AstNode) = "N"+ prefix

  private def digLabel(prefix: String, n: AstNode) = "%s [label=\"%s\"]".format(digNode(prefix, n), token(n).token)

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

object Ast2DotA {
  def apply(node: AstNode) = new Ast2Dot(node)
}