package value

import expression.Expression
import expression.Identifier
import context.Environment

class Closure(val parameters: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
  def apply(args: List[Value]): Value = {
    val tempEnv = new Environment(defEnv)
    tempEnv.bulkPut(parameters, args)
    body.execute(tempEnv)
  }
}

object Closure {
  def apply(param: List[Identifier], body: Expression, defEnv: Environment) = new Closure(param, body, defEnv)
}