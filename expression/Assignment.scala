package expression

import context.TypeException
import context.Environment
import value.Variable
import value.Notification

case class Assignment(val vbl: Identifier, val update: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    var tempValue = vbl.execute(env) // get Value from Environment
    if(!tempValue.isInstanceOf[Variable]) throw new TypeException("Type must be variable") // check if Value is type of Variable, if not terminate immediately
    val tempValue2 = tempValue.asInstanceOf[Variable] // cast to Variable
    tempValue2.content = update.execute(env) // update new Value into Variable's content
    Notification.DONE
  }
}