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
    
    var let = let_in_end(identifier("x"),natural(),N(3),identifier("y"))
    
    println(SemanticAnalyzer.TypeCheck(let,Map[identifier,Type]()))
    
  }

}
