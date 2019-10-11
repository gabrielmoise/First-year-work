object Question3 {
  def main (args : Array[String]) = {
    var rectangle = new Rectangle(1,2)
    var slab = new Slab(rectangle)
    println(slab.dimension)
    println(slab.getArea)
    slab.dimension.width = 3
    println(slab.dimension)
    println(slab.getArea)
  }
}
