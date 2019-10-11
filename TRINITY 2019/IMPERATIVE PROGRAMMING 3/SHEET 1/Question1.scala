class Shape (private val id : String){
  def getType : String = id
}

class Rectangle (private var width : Double, private var height : Double) extends Shape("rectangle") {
  def retrieve : (Double,Double) = (width,height)
  def change (newWidth : Double, newHeight : Double) : Unit = {
    width = newWidth
    height = newHeight
  }
}

class Square (private var size : Double) extends Shape("square") {
  def retrieve : Double = size
  def change (newSize : Double) : Unit = size = newSize
}

class Ellipse (private var major : Double, private var minor : Double) extends Shape("ellipse") {
  def retrieve : (Double,Double) = (major,minor)
  def change (newMajor : Double, newMinor : Double) : Unit = {
    major = newMajor
    minor = newMinor
  }
}

class Circle (private var radius : Double) extends Shape("circle") {
  def retrieve : Double = radius
  def change (newRadius : Double) : Unit = radius = newRadius
}
