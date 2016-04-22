package org.cg.adscraper.exprFilter

private class Part(val structure: String, val labels: String) {
  def combined = "%s\n\n%s\n".format(structure, labels)
}

class Ast2Dot(val node: AstNode) {

  def get(): String = "graph g{\n\n" + get(TokenLabeller(node).label()).combined + "\n}"

  private def get(node: Labelled): Part = {
    node match {
      case LTerminal(_, _) => new Part("", graphLabel(node))
      case LNonTerminal(_, _, children) => {
        val structure = "%s -- {%s}".format(graphNode(node), children.map((x) => graphNode(x)).mkString("; "))
        val label = graphLabel(node)

        val subs = children.map((x) => get(x))
        val subStructures = subs.
          filter((x) => x.structure.length() > 0).
          map(x => x.structure)
        val subLabels = subs.map(x => x.labels)

        new Part((structure :: subStructures).mkString("\n"), (label :: subLabels).mkString("\n"))
      }
    }
  }

  private def graphNode(n: Labelled) = "N" + n.label

  private def graphLabel(n: Labelled) = "%s [label=\"%s\"]".format(graphNode(n), n.token.value())

}

object Ast2Dot {
  def apply(node: AstNode) = new Ast2Dot(node)
}