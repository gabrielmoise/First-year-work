class Rectangle (private val width:Int, private val height:Int) {
/**
  def == (other : Rectangle) : Boolean =
    this.width == other.width && this.height == other.height
*/

  override def equals (other: Any) : Boolean =
    other match {
      case oth : Rectangle => this.width == oth.width && this.height == oth.height
      case _ => false
    }

  override def hashCode = (width,height).##
}

object Question7 {
  def main (args : Array[String]) = {
    // Tests for equality method
    var r1 = new Rectangle(20,30)
    var r2 = new Rectangle(20,30)
    var r3 = new Rectangle(20,30)
    println(r1 == r1)
    println(r1 == r2, r2 == r1)
    println(r1 == r2, r2 == r3, r3 == r1)
    var r = null : Rectangle
    println(r == null)
    println(r1 == null)
    val set = scala.collection.mutable.HashSet(r1)
    println(set.contains(r2))
  }
}
