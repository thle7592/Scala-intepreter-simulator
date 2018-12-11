package expression

import context.Environment
import value.Value

case class Block (val exps: List[Expression]) extends SpecialForm {
  def execute(env: Environment) = {
    val tempEnv = new Environment(env)
    // execute exps in tempEnv
    def helper(list: List[Expression], lastValue: Value = null) : Value = {
      if(list == Nil) lastValue
      else helper(list.tail, list.head.execute(tempEnv))
    }
    helper(exps)
  }
}