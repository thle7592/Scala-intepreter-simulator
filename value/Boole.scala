package value

case class Boole(val value: Boolean) extends expression.Literal {
  def &&(other: Boole) = Boole(this.value && other.value)
  def ||(other: Boole) = Boole(this.value || other.value)
  def unary_! = Boole(!value)
  
  override def toString = this.value.toString
  
  //override def execute(env: context.Environment) = new Boole(value)
}

