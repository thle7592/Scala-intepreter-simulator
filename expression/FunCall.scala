package expression
import context._
import value._

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression 
{
  def execute(env: Environment): Value = {    
    val args = operands.map(_.execute(env))
    if (env.contains(operator) && operator.execute(env).isInstanceOf[Closure])
      operator.execute(env).asInstanceOf[Closure](args)
    else 
      alu.execute(operator, args)
  }
}