package expression

trait Expression {
  def execute(env: context.Environment): value.Value
}