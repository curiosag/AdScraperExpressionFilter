package org.cg.adscraper.exprFilter

sealed abstract class AstNode

case class AstStructuralNonTerminal(name: String, children: List[AstNode]) extends AstNode
case class AstNonTerminal(symbol: Token, children: List[AstNode]) extends AstNode
case class AstTerminal(symbol: Token) extends AstNode

object AstEvaluator extends ExprEvaluator[AstNode] {
  def trace(v: String) = { System.out.println(v) }
  
  
  override def evalConst(const: Id): EvalResult[AstNode] = {
    EvalOk(AstTerminal(const))
  }

  override def evalUnOp(arg: EvalResult[AstNode], op: Op): EvalResult[AstNode] = {
    EvalOk(AstNonTerminal(op, List(evalSub(arg))))
  }

  override def evalRelOp(val1: Token, op: Op, val2: Token): EvalResult[AstNode] = {
    EvalOk(AstNonTerminal(op, List(AstTerminal(val1), AstTerminal(val2))))
  }

  override def evalBinOpBoolean(arg1: EvalResult[AstNode], op: Op, arg2: EvalResult[AstNode]): EvalResult[AstNode] = {
    EvalOk(AstNonTerminal(op, List(evalSub(arg1), evalSub(arg2))))
  }

  private def evalSub(arg1: EvalResult[AstNode]): AstNode =
    {
      arg1 match {
        case EvalOk(v) => v
        case EvalFail(_) => throw new RuntimeException // shouldn't happen
      }
    }

  override def evalFunc(name: Id, params: List[Token]): EvalResult[AstNode] = {
    EvalOk(AstStructuralNonTerminal("function call",
      List(AstStructuralNonTerminal("function name", List(AstTerminal(name))),
        AstStructuralNonTerminal("parameter list", params.map((x) => AstTerminal(x))))))
  }

  def print(node: AstNode): String = {
    {
      node match {
        case AstTerminal(v) => v.token
        case AstNonTerminal(v, children) => "%s (%s)".format(v.token, children.foldLeft("")((x, y) => x + " " + print(y)))
        case AstStructuralNonTerminal(name, children) => print(AstNonTerminal(Id(name), children))
         
      } 
    }
  }
  

  
  
  
}