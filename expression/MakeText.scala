package expression

import context.Environment
import value.Text

case class MakeText(val body: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    new Text(body)
  }
}