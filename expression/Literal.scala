package expression

import value.Value

trait Literal extends Expression with Value {
  def execute (env : context.Environment)= this
}