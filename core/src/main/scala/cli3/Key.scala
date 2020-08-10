package cli3


sealed trait Key {

  def =:=(another: Key): Boolean

  def canEqual(x: Any): Boolean = x.isInstanceOf[Key]

  override def equals(x: Any): Boolean = canEqual(x) && this =:= x.asInstanceOf[Key]
}

object Key {

  final case class Short(key: Char) extends Key {
    override def toString: String = s"-$key"
    def =:=(another: Key): Boolean = another match {
      case Short(`key`) => true
      case Full(`key`, _) => true
      case _ => false
    }
    override def hashCode(): Int = key.hashCode()
  }

  final case class Long(key: String) extends Key {
    override def toString: String = s"--$key"
    def =:=(another: Key): Boolean = another match {
      case Long(`key`) => true
      case Full(_, `key`) => true
      case _ => false
    }
    override def hashCode(): Int = key.hashCode()
  }

  final case class Full(short: Char, long: String) extends Key {
    override def toString: String = s"-$short|--$long"
    def =:=(another: Key): Boolean = another match {
      case Short(`short`) => true
      case Long(`long`) => true
      case Full(`short`, _) => true
      case Full(_, `long`) => true
      case _ => false
    }
    override def hashCode(): Int = short.hashCode()
  }
}