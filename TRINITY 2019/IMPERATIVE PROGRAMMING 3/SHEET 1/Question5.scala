class Ellipse (private var _a: Int, private var _b: Int) {

  def a = _a
  def a_= (a: Int) : Unit = {_a = a}

  def b = _b
  def b_= (b: Int) : Unit = {_b = b}

  def swap : Unit = {
    var t = _a
    _a = _b
    _b = t
  }
}

class LoggedEllipse (private var __a : Int, private var __b : Int) extends Ellipse (__a,__b) {

  private var incr : Int = 0

  // DTI : incr = # increases in the area of the Ellipse (when either _a or _b get increased)

  override def a_= (aa: Int) : Unit = {if (aa>a) incr += 1 ; super.a_=(aa)}

  override def b_= (bb: Int) : Unit = {if (bb>b) incr += 1 ; super.b_=(bb)}

  def getIncrease : Int = incr
}

object Question5 {
  def main (args : Array[String]) = {
      var ellipse = new LoggedEllipse(3,5)
      ellipse.a_=(4) //(4,5)
      ellipse.b_=(2) //(4,2)
      ellipse.b_=(6) //(4,6)
      ellipse.a_=(1) //(1,6)
      ellipse.swap   //(6,1)
      ellipse.a_=(5) //(5,1)
      println(ellipse.a) // 5
      println(ellipse.b) // 1
      println(ellipse.getIncrease) // 2
  }
}
