package expression
import context._
import value._

case class Conditional(val condition: Expression, val consequence: Expression, val alternative: Expression = Chars("")) extends SpecialForm
{
  def execute(env: Environment): Value = {
    if (condition.execute(env) == Boole(true)) consequence.execute(env)
    else {
      if (alternative == Chars("")) Notification.UNSPECIFIED
      else alternative.execute(env)
    }
  }
}