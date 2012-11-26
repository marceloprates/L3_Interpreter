/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package l3_interpreter

object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = 
  {
    var e1 = operation(N(5),N(43),plus())
    var e2 = operation(B(true),N(52),plus())
    var e3 = operation(N(5),N(1),greaterOrEqual())
    var e4 = operation(N(30),B(false),greaterOrEqual())
    var e5 = try_with(raise(),N(3))
    var e6 = define_function(identifier("x"),define_function(identifier("y"),operation(identifier("x"),identifier("y"),plus())))
    var e7 = define_function(identifier("x"),if_then_else(operation(N(0),identifier("x"),greaterOrEqual()),identifier("x"),operation(identifier("x"),N(-1),plus())))
    
    var e8 = apply_function(identifier("x"),B(true))
    var e9 = apply_function(identifier("x"),N(3))
    var e10 = let_in_end(identifier("z"),e9,e8)
    var e11 = define_function(identifier("y"),identifier("y"))
    var e12 = let_in_end(identifier("x"),e11,e9)
    //var e8 = let_in_end(identifier("x"),define_function(identifier("y"),identifier("y")),let_in_end(identifier("z"),apply_function(identifier("x"),N(3)),apply_function(identifier("x"),B(true))))
    
    //var exprs = List(apply_function(e7,N(0)),apply_function(e7,N(-1)),apply_function(e7,N(78)))
    
    var exprs = List(let_in_end(identifier("x"),N(3),let_in_end(identifier("y"),N(4),operation(identifier("x"),identifier("y"),plus()))))
    
    for(e <- exprs)
      {
        var t = SemanticAnalyzer.TypeInfer(e)
        
        var v = t match
        {
          case Some(x) => Some(SemanticAnalyzer.Eval(e))
          case None => None
        }
        
        //println("%s".format(SemanticAnalyzer.TypeCheck(e, Map[identifier,Type]())))
        println("%s | %s | %s".format(e,t,v))
      }
  }

}
