package expression

import context.Environment
import value.Thunk
import value.Text

case class Identifier(val name: String) extends Expression {
   override def toString = name
   def execute(env: Environment) = {
     val temp = env(this) // get value in hashMap  by key Identifier object
     if(temp.isInstanceOf[Thunk]) temp.asInstanceOf[Thunk].thunk()
     else if(temp.isInstanceOf[Text]) temp.asInstanceOf[Text].body.execute(env)
     else temp
   }
}