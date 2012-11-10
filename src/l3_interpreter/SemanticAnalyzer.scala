/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package l3_interpreter

class SemanticAnalyzer 
{
  
}

abstract class Type
case class natural extends Type
case class boolean extends Type
case class unit extends Type
case class function(t1: Type, t2: Type) extends Type
case class tuple(t1: Type, t2: Type) extends Type
case class record(labels_and_types: (identifier,Type)*) extends Type
case class reference(t: Type) extends Type



abstract class Expr
case class N(n: Int) extends Expr
case class B(b: Boolean) extends Expr
case class op(e1: Expr, e2: Expr) extends Expr
case class if_then_else(e1: Expr, e2: Expr, e3: Expr) extends Expr
case class attribution(e1: Expr, e2: Expr) extends Expr
case class dereferenciation(e: Expr) extends Expr
case class referenciation(e: Expr) extends Expr
//case class l() extends Expr
case class skip extends Expr
case class sequence(e1: Expr, e2: Expr) extends Expr
case class while_do(e1: Expr, e2: Expr) extends Expr
case class define_function(x: identifier, t: Type, e: Expr) extends Expr
case class apply_function(e1: Expr, e2: Expr) extends Expr
case class identifier(id: String) extends Expr
case class let_in_end(x: identifier, t: Type, e1: Expr, e2: Expr) extends Expr
//case class let_rec_in_end(f: Identifier, t1: Type, t2: Type, y: Identifier, e1: Expr, e2: Expr) extends Expr
case class define_tuple(e1: Expr, e2: Expr) extends Expr
case class project_1(e: Expr) extends Expr
case class project_2(e: Expr) extends Expr
case class define_record(labels_and_expressions: (Expr,Type)*) extends Expr
case class project_label(lab: identifier, e: Expr) extends Expr
case class raise extends Expr
case class try_with(e1: Expr, e2: Expr) extends Expr
