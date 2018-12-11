package expression

import value._

case class Declaration (val id: Identifier, val exp: Expression) extends SpecialForm {
  def execute(env: context.Environment) : Value = {
    val value = exp.execute(env)
    try{
      env(id) = value
      Notification.OK
    } catch {
      case _: Throwable => Notification.UNSPECIFIED
    }
  }
}