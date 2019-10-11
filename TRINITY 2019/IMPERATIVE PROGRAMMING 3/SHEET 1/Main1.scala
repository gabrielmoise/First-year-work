object Question1 {

  def identifySquares (set : Array[Shape]) : Array[Shape] = {
    var N = set.size
    var squares = new Array[Shape](N)
    var k = 0
    for (i <- 0 until N)
      set(i) match {
        case Rectangle(w,h) => if (w==h) {squares(k) = set(i) ; k += 1}
        case Square(s) => {squares(k) = set(i) ; k += 1}
        case _ => {}
      }
    squares
  }

  def identifyCircles (set : Array[Shape]) : Array[Shape] = {
    var N = set.size
    var circles = new Array[Shape](N)
    var k = 0
    for (i <- 0 until N)
      set(i) match {
        case Ellipse(major,minor) => if (major==minor) {circles(k) = set(i) ; k += 1}
        case Circle(s) => {circles(k) = set(i) ; k += 1}
        case _ => {}
      }
    circles
  }


  def main (args : Array[String]) = {
    var rectangle = new Rectangle (20.0,15.0)
    println(rectangle.retrieve._1+ " " +rectangle.retrieve._2)
    rectangle.change(2.0,5.0)
    println(rectangle.retrieve._1+ " " +rectangle.retrieve._2)
    var shapes = new Array[Shape](10)
    shapes(0) = new Rectangle(2.0,2.0)
    shapes(1) = new Ellipse(3.0,2.0)
    shapes(2) = new Square(2.0)
    shapes(3) = new Rectangle(9.0,11.0)
    shapes(4) = new Circle(5.0)
    shapes(5) = new Ellipse(8.5,8.5)
    var squares = identifySquares(shapes)
    println("The squares are:")
    var i = 0
    while ((i < squares.size) && (squares(i) != null)) {println(squares(i)) ; i += 1}
    var circles = identifyCircles(shapes)
    println("The circles are:")
    i = 0
    while ((i < circles.size) && (circles(i) != null)) {println(circles(i)) ; i += 1}
  }
}
