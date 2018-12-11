package value

class Notification (val msg: String) extends Value {
  override def toString = msg
}

object Notification {
  val OK = new Notification("OK")
  val DONE = new Notification("DONE")
  val UNSPECIFIED = new Notification("UNSPECIFIED")
  def apply(msg: String) = new Notification(msg)
}