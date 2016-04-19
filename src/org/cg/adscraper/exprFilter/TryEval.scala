package org.cg.adscraper.exprFilter

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

import scala.tools.nsc.interpreter.IMain;

object Eval {
  
  def main(args: Array[String]): Unit = {
    //  Create the script engine
    val javaxEngine = new ScriptEngineManager().getEngineByName("scala")
    val scalaEngine = javaxEngine.asInstanceOf[IMain]
  
    //scalaEngine.put("n: Int", 10);
    System.out.println("---");
    scalaEngine.eval("val a = 1");
  }
  
}