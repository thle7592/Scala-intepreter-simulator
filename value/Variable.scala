package value

class Variable (var content: Value) extends Value { // content is update-able
  // toString needed
  override def toString() = "[" + content + "]"
}