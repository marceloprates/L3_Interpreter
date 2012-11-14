/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package l3_interpreter

object SemanticAnalyzer 
{
  def TypeCheck(e: Expr, gamma: Map[identifier,Type]): Option[Type] =
    {
      e match
      {
        case N(n) => Some(natural())
        case B(b) => Some(boolean())
        case op(e1,e2,op:plus) =>
          (TypeCheck(e1,gamma),TypeCheck(e2,gamma)) match
          {
            case (Some(natural()),Some(natural())) => Some(natural())
            case _ => None
          }
        case op(e1,e2,op:greaterOrEqual) =>
          (TypeCheck(e1,gamma),TypeCheck(e2,gamma)) match
          {
            case (Some(natural()),Some(natural())) => Some(boolean())
            case _ => None
          }
        case if_then_else(e1,e2,e3) =>
          (TypeCheck(e1,gamma),TypeCheck(e2,gamma),TypeCheck(e3,gamma)) match
          {
            case (Some(boolean()),Some(t1),Some(t2)) => if(t1 == t2) Some(t1) else None
            case _ => None
          }
        case sequence(e1,e2) =>
          (TypeCheck(e1,gamma),TypeCheck(e2,gamma)) match
          {
            case (Some(unit()), Some(t)) => Some(t)
            case _ => None
          }
        case while_do(e1,e2) =>
          (TypeCheck(e1,gamma),TypeCheck(e2,gamma)) match
          {
            case (Some(boolean()), Some(unit())) => Some(unit())
            case _ => None
          }
        case identifier(id) =>
          {
            if(gamma.contains(identifier(id))) 
              Some(gamma(identifier(id))) 
            else 
              None
          }
        case define_function(x,t1,e) =>
          TypeCheck(e,gamma ++ Map(x -> t1)) match
          {
            case Some(t2) => Some(function(t1,t2))
            case _ => None
          }
        case apply_function(e1,e2) =>
          (TypeCheck(e1,gamma),TypeCheck(e2,gamma)) match
          {
            case (Some(function(t1,t2)),Some(t3)) => if(t1 == t3) Some(t2) else None
            case _ => None
          }
        case let_in_end(x,t,e1,e2) =>
          (TypeCheck(e1,gamma), TypeCheck(e2, gamma ++ Map(x -> t))) match
          {
            case (Some(t1),Some(t2)) => if(t1 == t) Some(t2) else None
            case _ => None
          }
        case let_rec_in_end(f,t1,t2,y,e1,e2) =>
          (TypeCheck(e1, gamma ++ Map(f -> function(t1,t2), y -> t1)), TypeCheck(e2, gamma ++ Map(f -> function(t1,t2)))) match
          {
            case (Some(t3),Some(t4)) => if(t3 == t2) Some(t4) else None
            case _ => None
          }
        case define_tuple(e1,e2) =>
          (TypeCheck(e1,gamma), TypeCheck(e2,gamma)) match
          {
            case (Some(t1),Some(t2)) => Some(tuple(t1,t2))
            case _ => None
          }
        case project_1(e) =>
          TypeCheck(e,gamma) match
          {
            case Some(tuple(t1,t2)) => Some(t1)
            case _ => None
          }
        case project_2(e) =>
          TypeCheck(e,gamma) match
          {
            case Some(tuple(t1,t2)) => Some(t2)
            case _ => None
          }
        case define_record(labels_and_expressions) =>
          {
            var labels = labels_and_expressions map (x => x._1)
            var types_or_nones = labels_and_expressions map (x => TypeCheck(x._2,gamma))
            
            if(types_or_nones.contains(None))
              {
                return None
              }
            else
              {
                var types = types_or_nones map (x => x match{case Some(t) => t})
                
                return Some(record(labels zip types))
              }
          }
        case project_label(lab,e) =>
          TypeCheck(e,gamma) match
          {
            case Some(record(labels_and_types)) =>
              {
                labels_and_types.find(x => x._1.equals(lab)) match
                {
                  case Some((l,t)) => Some(t)
                  case _ => None
                }
              }
            case _ => None
          }
        case attribution(e1,e2) =>
          (TypeCheck(e1,gamma),TypeCheck(e2,gamma)) match
          {
            case (Some(reference(t1)), Some(t2)) => if(t1 == t2) Some(unit()) else None
            case _ => None
          }
        case dereferenciation(e) =>
          TypeCheck(e,gamma) match
          {
            case Some(reference(t)) => Some(t)
            case _ => None
          }
        case referenciation(e) =>
          TypeCheck(e,gamma) match
          {
            case Some(t) => Some(reference(t))
            case _ => None
          }
        //case l()
        case raise() => Some(Type())
        case try_with(e1,e2) =>
          (TypeCheck(e1,gamma),TypeCheck(e2,gamma)) match
          {
            case (Some(t1),Some(t2)) => if(t1 canEqual t2) Some(t2) else None
            case _ => None
          }
          
          
          
      }
    }
}

case class Type
case class natural extends Type
case class boolean extends Type
case class unit extends Type
case class function(t1: Type, t2: Type) extends Type
case class tuple(t1: Type, t2: Type) extends Type
case class record(labels_and_types: List[(identifier,Type)]) extends Type
{
  def this(labels_and_types: (identifier,Type)*) = this(labels_and_types.toList)
}
case class reference(t: Type) extends Type
case class any() extends Type

abstract class Expr
case class N(n: Int) extends Expr
case class B(b: Boolean) extends Expr
case class op(e1: Expr, e2: Expr, op: Operator) extends Expr
case class if_then_else(e1: Expr, e2: Expr, e3: Expr) extends Expr
case class attribution(e1: Expr, e2: Expr) extends Expr
case class dereferenciation(e: Expr) extends Expr
case class referenciation(e: Expr) extends Expr
case class l(address: Int) extends Expr
case class skip extends Expr
case class sequence(e1: Expr, e2: Expr) extends Expr
case class while_do(e1: Expr, e2: Expr) extends Expr
case class define_function(x: identifier, t: Type, e: Expr) extends Expr
case class apply_function(e1: Expr, e2: Expr) extends Expr
case class identifier(id: String) extends Expr
case class let_in_end(x: identifier, t: Type, e1: Expr, e2: Expr) extends Expr
case class let_rec_in_end(f: identifier, t1: Type, t2: Type, y: identifier, e1: Expr, e2: Expr) extends Expr
case class define_tuple(e1: Expr, e2: Expr) extends Expr
case class project_1(e: Expr) extends Expr
case class project_2(e: Expr) extends Expr
case class define_record(labels_and_expressions: List[(identifier,Expr)]) extends Expr
{
  def this(labels_and_expressions: (identifier,Expr)*) = this(labels_and_expressions.toList)
}
case class project_label(lab: identifier, e: Expr) extends Expr
case class raise extends Expr
case class try_with(e1: Expr, e2: Expr) extends Expr

abstract class Operator
case class plus extends Operator
case class greaterOrEqual extends Operator