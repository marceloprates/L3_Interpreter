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
    var sum_1 = operation(N(5),N(43),plus())
    var sum_2 = operation(B(true),N(52),plus())
    var goe_1 = operation(N(5),N(1),greaterOrEqual())
    var goe_2 = operation(N(30),B(false),greaterOrEqual())
    var trywith_1 = try_with(raise(),N(3))
    
    var exprs = List(sum_1,sum_2,goe_1,goe_2,trywith_1)
    
    exprs.foreach(x => println("%s | %s | %s".format(x,SemanticAnalyzer.TypeInfer(x),SemanticAnalyzer.Eval(x))))
  }

}
