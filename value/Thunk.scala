package value

import context.Environment
import expression.Expression
import expression.Identifier
import context.Environment

class Thunk(override val body: Expression,override val defEnv: Environment) extends Closure(Nil, body, defEnv) {  
  var cache : Value = null
  def thunk() : Value = {
    if(cache == null) cache = super.apply(Nil)
    else null
    cache
  }
}

object Thunk {
  def apply(body: Expression, env: Environment) : Value = new Thunk(body, env)
}