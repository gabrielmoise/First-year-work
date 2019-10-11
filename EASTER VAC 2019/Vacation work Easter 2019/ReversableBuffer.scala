trait ReversableBuffer
{
  // State : B = list of data
  // Init : B = {}

  // add x to the end of this buffer
  // Post : B = B0 ++ [x]
  def append (x : Int)

  // add x to the start of this buffer
  // Post : B = x : B0
  def prepend (x : Int)

  // remove and return the i-th element, counting from zero
  // Pre : length B0 >= i
  // Post : return B0 !! i && B = take i B0 ++ drop (i+1) B0
  def get (i: Int) : Int

  // reverse the contents of the buffer
  // Post : B = reverse B0
  def rev
}
