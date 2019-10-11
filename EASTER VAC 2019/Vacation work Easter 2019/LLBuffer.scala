// Representing a buffer using a doubly-linked list with dummy headers (head and end)

// Let L(a,b) = [] if a=b; a:L(a.next,b), if a != b, and L(a) = L(a,null)

// Abstraction function : buffer = {b.datum | b is in init (L(buffer.next))}
// DTI : init(L(buffer.next)) is finite

class LLBuffer extends ReversableBuffer {
  private var buffer = new LLBuffer.Node(0,null,null)
  private var end = new LLBuffer.Node(0,null,null)
  buffer.next = end
  end.prev = buffer
  // We used a double-linked list with dummy headers to have append and prepend in O(1)

  // add x to the end of this buffer
  def append (x : Int) = {
      var n1 = new LLBuffer.Node(x,end.prev,end)
      end.prev.next = n1
      end.prev = n1
  }

  // add x to the start of this buffer
  def prepend (x : Int) = {
      var n1 = new LLBuffer.Node(x,buffer,buffer.next)
      buffer.next.prev = n1
      buffer.next = n1
  }

  // remove and return the i-th element, counting from zero
  // Pre : the length of the list is at least i
  // O(size)
  def get (i : Int) : Int = {
      var j = 0
      var current = buffer.next
      // current.datum is the jth element in the list
      while (i > j)
      {
        current = current.next
        j += 1
      }
      // i == j => we return current.datum and we remove it from the list
      var result = current.datum
      current.prev.next = current.next
      current.next.prev = current.prev
      result
  }

  // reverse the contents of the buffer
  // We swap the first element with the last one, then the next ones and so on until we meet the two ends
  // O(size)
  def rev = {
      var start = buffer.next
      var stop = end.prev
      if (start.prev != stop)
      while (start != stop)
      {
        // we know that there are 2 cases, either start.next = stop, or they have at least one node between them
        var aux = start.datum
        start.datum = stop.datum
        stop.datum = aux
        start = start.next
        stop = stop.prev
        if (start.prev == stop) // if they were adjacent
            start = stop // so that we exit the loop
        // else we keep going
      }
  }
}

// Companion object
object LLBuffer{
  private class Node (var datum : Int, var prev : Node, var next : Node)
}
