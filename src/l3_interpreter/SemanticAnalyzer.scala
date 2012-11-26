/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package l3_interpreter

object SemanticAnalyzer 
{
  var ftvCount = 0
  var typeSchemasCount = 0;
  
  def TypeCheck(e: Expr, gamma: Map[identifier,Type]): Option[(Type,Set[type_equation])] =
    {
      e match
      {
        case N(n) => Some(natural(),Set[type_equation]())
        case B(b) => Some(boolean(),Set[type_equation]())
        case operation(e1,e2,op:plus) =>
          (TypeCheck(e1,gamma),TypeCheck(e2,gamma)) match
          {
            case (Some((t1,c1)),Some((t2,c2))) => Some((natural(),c1 ++ c2 ++ Set(type_equation(t1,natural()), type_equation(t2,natural()))))
            case _ => None
          }
        case operation(e1,e2,op:greaterOrEqual) =>
          (TypeCheck(e1,gamma),TypeCheck(e2,gamma)) match
          {
            case (Some((t1,c1)),Some((t2,c2))) => Some((boolean(),c1 ++ c2 ++ Set(type_equation(t1,natural()), type_equation(t2,natural()))))
            case _ => None
          }
        case if_then_else(e1,e2,e3) =>
          (TypeCheck(e1,gamma),TypeCheck(e2,gamma),TypeCheck(e3,gamma)) match
          {
            case (Some((t1,c1)),Some((t2,c2)),Some((t3,c3))) => Some((t2, c1 ++ c2 ++ c3 ++ Set(type_equation(t1,boolean()),type_equation(t2,t3))))

            case _ => None
          }
        case identifier(id) =>
          {
            if(gamma.contains(identifier(id))) 
              Some((gamma(identifier(id)),Set[type_equation]()))
            else 
              None
          }
        case define_function(x,e) =>
          {
            var X = NewFTV()
            
            TypeCheck(e,gamma ++ Map(x -> X)) match
            {
              case Some((t,c)) => Some((function(X,t),c))
              case _ => None
            }
          }
        case apply_function(e1,e2) =>
          {
            var X = NewFTV()
            
            (TypeCheck(e1,gamma),TypeCheck(e2,gamma)) match
            {
              case (Some((t1,c1)),Some((t2,c2))) => Some((X, c1 ++ c2 ++ Set(type_equation(t1,function(t2,X)))))
              case _ => None
            }
          }
        case let_in_end(x,e2,e3) =>
          {
            var X = NewFTV()
            
            TypeCheck(e2,gamma ++ Map(x -> X)) match
            {
              case Some((t,c)) =>
                { 
                  TypeCheck(e3,gamma ++  Map(x -> X))
                }
              case None =>
                {
                  None
                }
            }
          }
        case raise() =>
          {
            var X = NewFTV()
            
            Some(X,Set[type_equation]())
          }
        case try_with(e1,e2) =>
          (TypeCheck(e1,gamma),TypeCheck(e2,gamma)) match
          {
            case (Some((t1,c1)),Some((t2,c2))) => Some((t2, c1 ++ c2 ++ Set(type_equation(t1,t2))))
            case _ => None
          }
      }
    }
  
  def TypeInfer(e: Expr): Option[Type] =
    {
      TypeCheck(e,Map[identifier,Type]()) match
      { 
        case Some((t,equations)) =>
          {
            Unify(equations) match
            {
              case Some(unified) =>
                {
                  var newT = t
                  
                  for(x <- unified)
                    {
                      newT = newT.Substitute(x._1, x._2)
                    }
                  
                  return Some(newT)
                }
              case None => None
            }
          }
        case None => None
      }
    }
  
  def TypeInfer(e: Expr, gamma: Map[identifier,Type]): Option[Type] =
    {
      TypeCheck(e,gamma) match
      { 
        case Some((t,equations)) =>
          {
            Unify(equations) match
            {
              case Some(unified) =>
                {
                  var newT = t
                  
                  for(x <- unified)
                    {
                      newT = newT.Substitute(x._1, x._2)
                    }
                  
                  return Some(newT)
                }
              case None => None
            }
          }
        case None => None
      }
    }
  
  def Step(e: Expr): Option[Expr] =
    {
      e match
          {
            // raise propagation rules
            
            case operation(raise(),raise(),op) => Some(raise())
            case operation(raise(),e2,op) => Some(raise())
            case operation(e1,raise(),op) => Some(raise())
              
            case if_then_else(raise(),raise(),raise()) => Some(raise())
            case if_then_else(raise(),raise(),e3) => Some(raise())
            case if_then_else(raise(),e2,raise()) => Some(raise())
            case if_then_else(raise(),e2,e3) => Some(raise())
            case if_then_else(e1,raise(),raise()) => Some(raise())
            case if_then_else(e1,raise(),e3) => Some(raise())
            case if_then_else(e1,e2,raise()) => Some(raise())
            
            case apply_function(raise(),raise()) => Some(raise())
            case apply_function(e1,raise()) => Some(raise())
            
            // regular rules
            
            case operation(e1,e2,op) =>
              {
                (e1,e2,op) match
                {
                  case (N(n1),N(n2),op:plus) => Some(N(n1+n2))
                  case (N(n1),N(n2),op:greaterOrEqual) => Some(B(n1 >= n2))
                  case (e1,e2,op) =>
                  {
                    if(e1 IsValue)
                    {
                      Step(e2) match
                      {
                        case Some(e2Lin) => Some(operation(e1,e2Lin,op))
                        case _ => None
                      }
                    }
                    else
                    {
                      Step(e1) match
                      {
                        case Some(e1Lin) => Some(operation(e1Lin,e2,op))
                        case _ => None
                      }
                    }
                  }
                  case _ => None
                }
              }
            case if_then_else(e1,e2,e3) =>
              e1 match
              {
                case B(true) => Some(e2)
                case B(false) => Some(e3)
                case e1 =>
                  Step(e1) match
                  {
                    case Some(e1Lin) => Some(if_then_else(e1Lin,e2,e3))
                    case _ => None
                  }
              }
            case apply_function(e1,e2) =>
              (e1 IsValue, e2 IsValue) match
              {
                case (true,true) =>
                  {
                    (e1,e2) match
                    {
                      case (define_function(x,e),v) => Some(e.Substitute(x,v))
                      case _ => None
                    }
                  }
                case (false,_) =>
                  {
                    Step(e1) match
                    {
                      case Some(e1Lin) => Some(apply_function(e1Lin,e2))
                      case _ => None
                    }
                  }
                case (true,false) =>
                  {
                    Step(e2) match
                    {
                      case Some(e2Lin) => Some(apply_function(e1,e2Lin))
                      case _ => None
                    }
                  }
              }
            case try_with(e1,e2) =>
              {
                if(e1 IsValue)
                {
                  Some(e1)
                }
                else
                {
                  e1 match
                  {
                    case raise() => Some(e2)
                    case _ =>
                      Step(e1) match
                      {
                        case Some(e1Lin) => Some(try_with(e1Lin,e2))
                        case _ => None
                      }
                  }
                }
              }
            case let_in_end(x,e1,e2) =>
              {
                Step(e1) match
                {
                  case Some(e1Lin) => Some(let_in_end(x,e1Lin,e2))
                  case None =>
                    if (e1 IsValue)
                      {
                        Some(e2.Substitute(x,e1))
                      }
                    else
                      {
                        None
                      }
                }
              }
            case _ => None
          }
    }
  
  def Eval(e: Expr): Option[Expr] =
    {
      Step(e) match
      {
        case Some(eLin) => Eval(eLin)
        case None => if(e IsValue) Some(e) else None
      }
    }
  
  def Substitute(X: ftv, t: Type, c: Set[type_equation]): Set[type_equation] =
    {
      var newC = Set[type_equation]()
      
      for(equation <- c)
        {
          if(!(equation.t1 equals X) || !(equation.t2 equals t))
            {
              newC = newC ++ Set(type_equation(equation.t1.Substitute(X,t),equation.t2.Substitute(X,t)))
            }
        }
      
      return newC
    }
    
  def Unify(c: Set[type_equation]): Option[Map[ftv,Type]] =
    {
       var equations = c
       var substitution = Set[type_equation]()
       
       while(!equations.isEmpty)
         {
           for(equation <- equations)
             {
               equation match
               {
                 case type_equation(function(t1,t2),function(t3,t4)) =>
                   {
                     equations = equations.filterNot(x => x equals equation)
                     equations = equations ++ Set(type_equation(t1,t3),type_equation(t2,t4))
                     //break
                   }
                 case type_equation(ftv(id),t) =>
                   {
                     (ftv(id) equals t, t.FTVs.contains(ftv(id))) match
                     {
                       case (true,_) =>
                         {
                           equations = equations.filterNot(x => x equals equation)
                         }
                       case (false,true) =>
                         {
                           return None
                         }
                       case (false, false) =>
                         {
                           substitution = Substitute(ftv(id),t,substitution) ++ Set(equation)
                           equations = equations.filterNot(x => x equals equation)
                           equations = Substitute(ftv(id),t,equations)
                           //break
                         }
                     }
                   }
                 case type_equation(t,ftv(id)) =>
                   {
                     (ftv(id) equals t, t.FTVs.contains(ftv(id))) match
                     {
                       case (true,_) =>
                         {
                           equations = equations.filterNot(x => x equals equation)
                         }
                       case (false,true) =>
                         {
                           return None
                         }
                       case (false, false) =>
                         {
                           substitution = Substitute(ftv(id),t,substitution) ++ Set(type_equation(ftv(id),t))
                           equations = Substitute(ftv(id),t,equations)
                           //break
                         }
                     }
                   }
                 case type_equation(function(t1,t2),t3) =>
                   {
                     return None
                   }
                 case type_equation(t1,function(t2,t3)) =>
                   {
                     return None
                   }
                 case type_equation(t1,t2) =>
                   {
                     if(t1 equals t2)
                       {
                         equations = equations.filterNot(x => x equals equation)
                       }
                       else if(t1.FTVs.isEmpty && t2.FTVs.isEmpty)
                       {
                         return None
                       }
                   }
                 case _ => {}
               }
             }
         }
    
       var substitution_map = substitution map (x => (x.t1 , x.t2)) toMap
    
       return Some(substitution_map.asInstanceOf[Map[ftv,Type]])
    }
   
  def NewFTV(): ftv =
    {
      ftvCount = ftvCount + 1
      
      return ftv(ftvCount.toString)
    }

  def NewTypeSchema(): typeSchema =
    {
      typeSchemasCount = typeSchemasCount + 1
      
      return typeSchema(typeSchemasCount.toString)
    }

  def WithTypeSchemas(t: Type): Type =
    {
      t match
      {
        case natural() => natural()
        case boolean() => boolean()
        case function(t1,t2) => function(WithTypeSchemas(t1),WithTypeSchemas(t2))
        case ftv(id) => NewTypeSchema()
      }
    }
}

abstract case class Type
{
  def FTVs(): Set[ftv] =
    {
      this match
      {
        case ftv(id) => Set(ftv(id))
        case natural() => Set[ftv]()
        case boolean() => Set[ftv]()
        case function(t1,t2) => t1.FTVs ++ t2.FTVs
        case typeSchema(id) => Set[ftv]()
      }
    }

  def Substitute(X: ftv, t: Type): Type =
    {
      this match
      {
        case natural() => natural()
        case boolean() => boolean()
        case function(t1,t2) => function(t1.Substitute(X,t),t2.Substitute(X,t))
        case ftv(id) => if(ftv(id) equals X) t else ftv(id)
        case typeSchema(id) => typeSchema(id)
      }
    }
}
case class natural extends Type
case class boolean extends Type
case class function(t1: Type, t2: Type) extends Type
case class ftv(id: String) extends Type
case class typeSchema(id: String) extends Type

case class type_equation(t1: Type, t2: Type)

abstract class Expr
{
  def IsValue(): Boolean =
  {
    this match
    {
      case N(n) => true
      case B(b) => true
      case define_function(x,e) => true
      case _ => false
    }
  }
  
  def Substitute(x: identifier, v: Expr): Expr =
    {
      this match
      {
        case N(n) => N(n)
        case B(b) => B(b)
        case operation(e1,e2,op) => operation(e1.Substitute(x,v),e2.Substitute(x,v),op)
        case define_function(y,e1) => 
        {
            if(y equals x)
            {
              define_function(y,e1)
            }
            else
            {
              define_function(y,e1.Substitute(x,v))
            }
        }
        case if_then_else(e1,e2,e3) => if_then_else(e1.Substitute(x,v),e2.Substitute(x,v),e3.Substitute(x,v))
        case apply_function(e1,e2) =>
        {
          apply_function(e1.Substitute(x,v),e2.Substitute(x,v))
        }
        case identifier(id) =>
        {
          if(identifier(id) equals x) 
            {
              v
            }
          else 
            {
              identifier(id)
            }
        }
        case let_in_end(y,e1,e2) =>
        {
          if(y equals x)
          {
            let_in_end(y,e1,e2)
          }
          else
          {
            let_in_end(y,e1.Substitute(x,v),e2.Substitute(x,v))
          }
        }
        case raise() => raise()
        case try_with(e1,e2) => try_with(e1.Substitute(x,v),e2.Substitute(x,v))
      }
    }
}
case class N(n: Int) extends Expr
case class B(b: Boolean) extends Expr
case class operation(e1: Expr, e2: Expr, op: Operator) extends Expr
case class if_then_else(e1: Expr, e2: Expr, e3: Expr) extends Expr
case class define_function(x: identifier, e: Expr) extends Expr
case class apply_function(e1: Expr, e2: Expr) extends Expr
case class identifier(id: String) extends Expr
{
  override def equals(o: Any): Boolean =
    {
      this.id equals (o.asInstanceOf[identifier]).id
    }
}
case class let_in_end(x: identifier, e1: Expr, e2: Expr) extends Expr
case class raise extends Expr
case class try_with(e1: Expr, e2: Expr) extends Expr

abstract class Operator
case class plus extends Operator
case class greaterOrEqual extends Operator