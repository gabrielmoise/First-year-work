class Triangle

class OpaqueTriangle extends Triangle

class Renderer {
  def accept(a: Triangle) = println("Accepted for rendering.")
}

class RayTracingRenderer extends Renderer {
  override def accept(a: Triangle) = println("Accepted for ray-trace rendering.")
}

object Question4 {
  def main (args : Array[String]) = {
    val a: Triangle = new OpaqueTriangle
    val r1: Renderer = new RayTracingRenderer
    r1.accept(a)
    val r2: RayTracingRenderer = new RayTracingRenderer
    r2.accept(a)
  }
}

/*
The output for this input is going to be
Accepted for rendering.
Accepted for ray-trace rendering.
This is due to the fact that r1 is considered to be of class OpaqueTriangle is a subclass
of the Triangle class, and using the "is a" rule, we can say that any OpagueTriangle object
is a Triangle, but not vice-versa. This means that, because r1 is of type Renderer, the method
which will be used is the one which returns "Accepted for rendering.", because in this class
there is not the other accept function (the RayTracingRenderer is a subclass of Renderer) and the
accept function is not overriden in the dynammic class.
The second output will be "Accepted for ray-trace rendering.", since r2 is defined to be a
RayTracingRenderer object, so the compiler has to decide which accept function to use, since
it now has two overloaded functions, and it will choose the one which receives an OpaqueTriangle
type argument, rather than a Triangle one, because of pattern-matching.
To change the first output to also be "Accepted for ray-trace rendering.", we imply need to
change the type of the argument that accept receives to Triangle, and to override it.
*/
/*
class RayTracingRenderer extends Renderer {
  override def accept(a: Triangle) = println("Accepted for ray-trace rendering.")
}
*/
