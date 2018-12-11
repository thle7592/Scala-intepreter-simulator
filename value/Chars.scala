package value

import expression.Literal
import context.Environment

case class Chars(val value: String) extends Literal with Ordered[Chars] with Equals {
  def substring(start: Integer, end: Integer = Integer(value.size)) = Chars(value.substring(start.value, end.value))
  def +(other: Chars) = Chars(this.value + other.value)
  
  override def toString = this.value.toString
  
  def compare(other: Chars): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  
  override def canEqual(other: Any) = other.isInstanceOf[Chars]
  override def equals(other: Any): Boolean = { 
    other match {
      case other: Chars => this.canEqual(other) && (other.hashCode == this.hashCode)
      case _ => false
    }
  }
  override def hashCode = this.value.##
  /*{ 
    val newHash = if(value == null || value == "") 0 else value.hashCode
    super.hashCode + newHash
  }*/
}