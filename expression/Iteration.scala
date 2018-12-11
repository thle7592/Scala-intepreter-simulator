package expression

import context.Environment
import context.TypeException
import value._

case class Iteration (val condition: Expression, val body: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    def looper(cond: Expression, exp: Expression) : Value = {
      if(!cond.execute(env).isInstanceOf[Boole]) throw new TypeException()
      
      if(cond.execute(env) == Boole(true)) { exp.execute(env); looper(cond, exp) }
      else Notification.DONE
    }
    looper(condition, body)
  }
}