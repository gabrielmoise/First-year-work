object Question1
{
  // SA NU UIT SA SCOT COMENTUL DE LA URMATOAREA LINIE PE CARE AM PUS-O DOAR PT CA SA POT COMPILA FARA ERORI
  // def swap (x : Int, y: Int) = {val t = x; x = y; y = t}
  // We will get an error here for reassigning x and y with new values. They have to stay the same because they are the arguments of the function
  // so they behave in the same way as val variables do
  // error: reassignment to val
  def swapEntries(a: Array[Int], i: Int, j: Int) = {
      val t = a(i); a(i) = a(j); a(j) = t
    }
  // Here we get no error as long as i and j do not exceed the length of the array.
  // This is because although a is a value array, we can modify its entries(here we interchange entry i with entry j)
}

// QUESTION 2
object SideEffects
{
    var x = 3; var y = 5
    def nasty (x: Int) : Int = { y = 1; 2 * x }
    def main (args: Array[String]) = println(nasty(x)+ y)
    // In this form, we first make y equal to 1 then we double x, so it becomes 6, then at printing we have 6 + 1 (the new value of y)
    // from the "nasty" function.
    // If we had println(y + nasty(x)), first we would have y = 5, then nasty (x), which returns 2*x = 6, so the integer returned would be 11.
    // The order matters as we perform the addition from left to right, calculating each function we need before performing the next addition.
}
