package expression

import context.Environment
import value.Closure

case class Lambda(val parameters: List[Identifier], val body: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    Closure(parameters, body, env)
  }
}