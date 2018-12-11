package expression
import context._
import value._

case class Disjunction(val operands: List[Expression]) extends SpecialForm
{
  def execute(env: Environment): Value =
  {
    def helper(lists: List[Expression]): Value = {
      if (lists == Nil) Boole(false)
      else if (lists.head.execute(env) == Boole(true)) Boole(true)
      else helper(lists.tail)
    } 
    helper(operands)
  }
}