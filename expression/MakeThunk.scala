package expression

import context.Environment
import value._

case class MakeThunk(val body: Expression) extends SpecialForm {
  def execute(env: Environment) : Value = {
     Thunk(body, env)
  }
}