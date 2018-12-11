package test

import context._
import expression._
import value._

object ExpTest extends App {
  val globalEnv = new Environment
  val pi = Identifier("pi")
  val num = Real(3.14)
  globalEnv.put(pi, num)
  println(pi.execute(globalEnv)) // execute method of Identifier is called
  println(num.execute(globalEnv)) // execute method of Literal is called
}