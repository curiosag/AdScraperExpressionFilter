package org.cg.adscraper.exprFilter

import scala.util.parsing.combinator.syntactical._;

class Token(val token: String) 
case class Id(override val token: String) extends Token(token)
case class Num(override val token: String) extends Token(token)
case class Op(override val token: String) extends Token(token)

class ExprParser[T](e: ExprEvaluator[T]) extends StandardTokenParsers {
  val cFalse = "false"
  val cTrue = "true"
  val cPasses = "passes"
  val cEq = "=="
  val cLt = "<"
  val cGt = ">"
  val cLe = "<="
  val cGe = ">="
  val cAnd = "&"
  val cOr = "|"
  val cNot = "!"

  lexical.delimiters += (cEq, cLt, cGt, cLe, cGe, "(", ")", cAnd, cOr, cNot, ",");
  lexical.reserved += (cFalse, cTrue, cPasses); // a reserved word can not be an identifier

  def binOp: Parser[String] = cEq | cLt | cGt | cLe | cGe

  /**
   * OR and AND have equal precedence this way
   *  <b-expression>::= <b-term> [OR|AND <b-term>]*
   *  <not-factor>  ::= [NOT] <b-factor>
   *  <b-factor>    ::= <b-literal> | <b-variable> | (<b-expression>)
   */

  def booleanExpr: Parser[EvalResult[T]] = bNotFactor ~ rep((cOr | cAnd) ~ bNotFactor) ^^ {
    case f ~ flist => flist.foldLeft(f)((current, next) => next match { case (op ~ rightResult) => e.evalBinOpBoolean(current, Op(op), rightResult) })
  }

  def bNotFactor: Parser[EvalResult[T]] = opt(cNot) ~ bFactor ^^ {
    case Some(not) ~ f => e.evalUnOp(f, Op(not))
    case None ~ f => f
  }

  def bFactor: Parser[EvalResult[T]] = bFunction | bConst | "(" ~> booleanExpr <~ ")"

  def bConst: Parser[EvalResult[T]] = (cFalse | cTrue) ^^ { case c => e.evalConst(c) }

  def numBinOp: Parser[EvalResult[T]] = term ~ binOp ~ term ^^ { case (v1 ~ op ~ v2) => e.evalRelOp(v1, Op(op), v2) }

  def numInterval: Parser[EvalResult[T]] = term ~ binOp ~ term ~ binOp ~ term ^^ { case (v1 ~ op1 ~ v2 ~ op2 ~ v3) => e.evalBinOpBoolean(e.evalRelOp(v1, Op(op1), v2), Op(cAnd), e.evalRelOp(v2, Op(op2), v3)) }

  def passesFilter: Parser[EvalResult[T]] = cPasses ~> "(" ~> id ~ "," ~ id <~ ")" ^^ { case (v1 ~ "," ~ v2) => e.evalPassFilter(v1, v2) }

  def term: Parser[Token] = id | num
  def id: Parser[Id] = ident ^^ { case x => Id(x) }
  def num: Parser[Num] = numericLit ^^ { case x => Num(x) }

  def bFunction: Parser[EvalResult[T]] = passesFilter | numInterval | numBinOp

  def tokenizer(source: String) = new lexical.Scanner(source)
  def parseResult(source: String) = phrase(booleanExpr)(tokenizer(source))

  def parse(src: String) = {
      val tokens = new lexical.Scanner(src)
      phrase(booleanExpr)(tokens) match 
      {
        case Success(result, _) => result
        case x => EvalFail(x.toString())
      }
  }
}
