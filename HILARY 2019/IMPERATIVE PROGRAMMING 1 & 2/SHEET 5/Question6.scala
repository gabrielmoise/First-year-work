class IntQueue
{
  // Abstraction function : queue = L(list.next), L(null) = {}, L(x) = x.datum:L(x.next)
  // DTI : L(list.next) is finite and ends in end

  private type Node = IntQueue.Node
  private def Node(datum:Int, next:Node) = new IntQueue.Node(datum,next)

  private var list = Node(0,null)
  private var end = Node(0,null)
  list.next = end

  // Instead of the dummy end we place the new node and we create a new dummy end afterwards, updating end
  def enqueue (x : Int) =
    {
      end.datum = x
      end.next = Node(0,null)
      end = end.next
    }

  // First, we need that the queue is not empty, which happens when isEmpty = true
  // Then, if not, we keep the data of the first node after the dummy header, and then we delete it
  def dequeue : Int =
    {
        require (! isEmpty)
        var result = list.next.datum
        list.next = list.next.next
        result
    }

  // The queue is empty if we have list.next = end
  def isEmpty : Boolean = (list.next == end)

}

// Companion object
object IntQueue{
  private class Node(var datum:Int, var next:Node)
}
