class Rectangle (var width : Int, var height : Int) {
  var area = width * height
  ...
}

The problems with this piece of code is :
- the width and height variables are not private and therefore they can be modified,
thus breaking the principle of encapsulation
- the width and height are of a wrong type, they should be Double variables because
the dimensions might be non-integer numbers
- there should be a datatype invariant, which would also incorporate the area relationship
between it and width*height, which is area = width* height. Thus, it can be violated since
the is declared as a variable and not as private one
- also, we might need this class to be a case class in case we want to use equality or
pattern-matching on it
- the area should be obtained by the user with a separate function, getArea, which should
just return the value, which is ought to be updated each time one of the dimensions is modified
in any context

My version would look like :

case class Rectangle (private var width : Double, private var height : Double) {
  // DTI : area = width * height && width > 0 && height > 0

  private var area = width * area

  // This should be returned immediately, without needing for a multiplication
  def getArea : Double = area

  ... (any function the modifies the dimensions changes the area too)
}
