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
        case raise() => Some(Type())
        case try_with(e1,e2) =>
          (TypeCheck(e1,gamma),TypeCheck(e2,gamma)) match
          {
            case (Some(t1),Some(t2)) => if(t1 canEqual t2) Some(t2) else None
            case _ => None
          }
      }
    }
  /*
  def Step(e: Expr): Option[Expr] =
  {
    TypeCheck(e,Map[identifier,Type]()) match
    {
      case None => None
      case _ =>
        e match
        {
          case op(N(n1),N(n2),op:plus) => Some(N(n1+n2))
          case op(N(n1),N(n2),op:greaterOrEqual) => Some(B(n1 >= n2))
          case op(e1,e2,op) =>
            (Step)
        }
    }
  }
  */ 

}

case class Type
case class natural extends Type
case class boolean extends Type
case class function(t1: Type, t2: Type) extends Type

abstract class Expr
case class N(n: Int) extends Expr
case class B(b: Boolean) extends Expr
case class op(e1: Expr, e2: Expr, op: Operator) extends Expr
case class if_then_else(e1: Expr, e2: Expr, e3: Expr) extends Expr
case class define_function(x: identifier, t: Type, e: Expr) extends Expr
case class apply_function(e1: Expr, e2: Expr) extends Expr
case class identifier(id: String) extends Expr
case class let_in_end(x: identifier, t: Type, e1: Expr, e2: Expr) extends Expr
case class raise extends Expr
case class try_with(e1: Expr, e2: Expr) extends Expr

abstract class Operator
case class plus extends Operator
case class greaterOrEqual extends Operator