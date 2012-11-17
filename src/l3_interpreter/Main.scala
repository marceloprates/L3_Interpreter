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
    var e1 = define_function(identifier("x"),operation(identifier("x"),N(3),plus()))
    
    var e2 = apply_function(e1,N(5))
    
    var e3 = operation(identifier("x"),N(3),plus())
    
    //println(e3.Substitute(identifier("x"), N(5)))
    
    println(SemanticAnalyzer.Eval(e2))//TypeInfer(e2))
  }

}
