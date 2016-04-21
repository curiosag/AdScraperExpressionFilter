package org.cg.adscraper.exprFilter

import scala.util.parsing.combinator.syntactical._;

class Token(val token: String)
case class Id(override val token: String) extends Token(token)
case class Num(override val token: String) extends Token(token)
case class Op(override val token: String) extends Token(token)

class ExprParser[T](e: ExprEvaluator[T]) extends StandardTokenParsers {
  private val cFalse = "false"
  private val cTrue = "true"
  private val cPasses = "passes"
  private val cEq = "=="
  private val cLt = "<"
  private val cGt = ">"
  private val cLe = "<="
  private val cGe = ">="
  private val cAnd = "&"
  private val cOr = "|"
  private val cNot = "!"

  lexical.delimiters += (cEq, cLt, cGt, cLe, cGe, "(", ")", cAnd, cOr, cNot, ",");
  lexical.reserved += (cFalse, cTrue)

  /**
   * OR and AND have equal precedence as defined here
   * 
   * identifiers: x, y
   * boolean literals: false, true
   * numeric literals: 1, -1, 1.1
   * boolean expressions: ! false & true | (false | x)
   * numeric comparisons and numeric intervals: ! 1 > 2 & 0 <= x < 100
   * function call syntax, allowing only identifiers as params: passes(refvalue, filterid) & someFct(a, x, y)
   * 
   *  <b-expression>::= <b-term> ["|"|& <b-term>]*
   *  <not-factor>  ::= [!] <b-factor>
   *  <b-factor>    ::= <booleanliteral> | <identifier> | numRelOp | numInterval | functionCall | (<b-expression>) 
   *  <functionCall>::= <identifier> (<identifier> [<listSep> <identifier>]*)
   *  <numRelOp> 	  ::= <numericTerminal> <relOp> <numericTerminal>
   *  <numInterval> ::= <numericTerminal> <relOp> <numericTerminal> <RelOp> <numericTerminal>
   *  <booleanliteral>   ::= true | false 
   *  <numericTerminal> ::= <identifier> | <numericLiteral>
   *  <relOp>				::= > | < | >= | <= | ==
   *  <listSep>			::= ,
   */

  
  private def binOp: Parser[String] = cEq | cLt | cGt | cLe | cGe
  
  private def booleanExpr: Parser[EvalResult[T]] = bNotFactor ~ rep((cOr | cAnd) ~ bNotFactor) ^^ {
    case f ~ flist => flist.foldLeft(f)((current, next) =>
      next match { case (op ~ rightResult) => e.evalBinOpBoolean(current, Op(op), rightResult) })
  }

  private def bNotFactor: Parser[EvalResult[T]] = opt(cNot) ~ bFactor ^^ {
    case Some(not) ~ f => e.evalUnOp(f, Op(not))
    case None ~ f => f
  }

  private def bFactor: Parser[EvalResult[T]] = bFunction | bConst | "(" ~> booleanExpr <~ ")"

  private def bConst: Parser[EvalResult[T]] = (cFalse | cTrue) ^^ { case c => e.evalConst(Id(c)) }

  private def numBinOp: Parser[EvalResult[T]] = term ~ binOp ~ term ^^
    { case (v1 ~ op ~ v2) => e.evalRelOp(v1, Op(op), v2) }

  private def numInterval: Parser[EvalResult[T]] = term ~ binOp ~ term ~ binOp ~ term ^^
    { case (v1 ~ op1 ~ v2 ~ op2 ~ v3) => e.evalBinOpBoolean(e.evalRelOp(v1, Op(op1), v2), Op(cAnd), e.evalRelOp(v2, Op(op2), v3)) }

  private def bFunctionCall: Parser[EvalResult[T]] = id ~ "(" ~ paramList ~ ")" ^^ 
  { case (n ~  "(" ~ p ~ ")") =>  e.evalFunc(n, p) }

  private def paramList: Parser[List[Token]] = opt(id ~ rep("," ~> id)) ^^
    {
      case Some(h ~ t) => (h :: t)
      case None => List()
    }
  
  private def bFunction: Parser[EvalResult[T]] = numInterval | numBinOp | bFunctionCall
  
  private def term: Parser[Token] = id | num
  private def id: Parser[Id] = ident ^^ { case x => Id(x) }
  private def num: Parser[Num] = numericLit ^^ { case x => Num(x) }
  
  def parse(src: String) = {
    val tokens = new lexical.Scanner(src)
    phrase(booleanExpr)(tokens) match {
      case Success(result, _) => result
      case x => EvalFail(x.toString())
    }
  }
}
