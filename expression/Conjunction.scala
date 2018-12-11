package expression
import context._
import value._

case class Conjunction(val operands: List[Expression]) extends SpecialForm
{
  def execute(env: Environment): Value =
  {
    def helper(lists: List[Expression]): Value = {
      if (lists == Nil) Boole(true)
      else if (lists.head.execute(env) == Boole(false)) Boole(false)
      else helper(lists.tail)
    }
    helper(operands)
  }
}