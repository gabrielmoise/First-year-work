// It can be observed from the specification that the class parameter of Bag is a function
class Bag[-T](private val f : (T => Int)) {
  /** Add an element to the bag */
  def add (x : T) : Bag[T] = new Bag[T](v => {if (x == v) f(v) + 1 else f(v)})

  /** Remove an element if it is in the bag */
  def remove (x : T) : Bag[T] = new Bag[T] (v => {if (x == v) Math.max(0,f(v)-1) else f(v)})

  /** Count how many times an element appears in the bag */
  def count (x : T) : Int = f(x)

  /** Return the union of two bags */
  def union [X <: T] (that : Bag[X]) : Bag[X] = new Bag[X] (v => f(v) + that.count(v))
}
