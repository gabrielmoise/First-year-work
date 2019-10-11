case class Rectangle (var width : Int, var height : Int)


class Slab (private val __dimension : Rectangle) {

  private val _dimension = new Rectangle (__dimension.width, __dimension.height)

  private val _area = _dimension.width * _dimension.height

  def dimension = new Rectangle (_dimension.width, _dimension.height)
}

/**
class Slab (private val _dimension : Rectangle) {
  private val _area = _dimension.width * _dimension.height

  def dimension = _dimension
}
*/
//The invariant can be invalidated this way (we take advantage of the fact that the width and heigth
//fields of the Rectangle class are not private, so we can modify them, and since the argument
// of a Slab object is a Rectangle object, we also modify it there and thus the DTI is violated)

object Question3 {
  def main (args : Array[String]) = {
    var rectangle = new Rectangle(1,2)
    var slab = new Slab(rectangle)
    println(slab.dimension)
    slab.dimension.width = 10
    println(slab.dimension)
  }
}

/**
The problem that is ilustrated in main is that the parameters of
the dimension of the Slab object can be modified, althgough they are private
to the user and this happens because when calling the dimension function from
Slab, we get the address of the Rectangle object and we can therefore modify it.
My solution would be to create new Rectangle object in the Slab class, so when
we modify the data from the dimension parameter we can't every time we ask for the
dimension of the object, it gets the original version from the Rectangle object,
and this is the effect we wanted(we shouldn't be able to modify it as a user due
to its private type).
*/
