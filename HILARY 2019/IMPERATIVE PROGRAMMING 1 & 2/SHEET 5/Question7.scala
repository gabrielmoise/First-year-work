class DoubleEndedQueue
{
  // Abstraction function : queue = L(list.next), L(null) = {}, L(x) = x.datum:L(x.next)
  // DTI: L(list.next) is finite and ends in end (we do not count the dummy end)

  private type Node = DoubleEndedQueue.Node
  private def Node(datum:Int, prev:Node, next:Node) = new DoubleEndedQueue.Node(datum,prev,next)

  private var list = Node (0,null,null)
  private var end = Node (0,null,null)
  list.next = end
  end.prev = list

  // state : s: seq Int
  // init : s = {}

  /** Is the queue empty? */
  // Post: list = list_0 && return list.next == end
  def isEmpty : Boolean = (list.next == end)

  /** add x to the start of the queue. */
  // Post : list = x : list_0
  def addLeft(x:Int) =
    {
      list.datum = x
      list.prev = Node(0,null,list)
      list = list.prev
    }

  /** get and remove element from the start of the queue. */
  // Pre : list is non-empty
  // Post : list = tail list_0 && return head list_0
  def getLeft : Int =
    {
      require (! isEmpty)
      var result = list.next.datum
      list.next = list.next.next
      list.next.prev = list
      result
    }

    /** add element to the end of the queue. */
    // Post : list = list_0 ++ [x]
  def addRight(x: Int) =
    {
      end.datum = x
      end.next = Node (0,end,null)
      end = end.next
    }

  /** get and remove element from the end of the queue. */
  // Pre : list is non-empty
  // Post : list = init list_0 && return last list_0
  def getRight : Int =
    {
      require (! isEmpty)
      var result = end.prev.datum
      end.prev = end.prev.prev
      end.prev.next = end
      result
    }
}

// Companion object
object DoubleEndedQueue{
  private class Node(var datum:Int, var prev:Node, var next:Node)
}
