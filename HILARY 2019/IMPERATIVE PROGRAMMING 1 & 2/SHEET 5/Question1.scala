object Question1
{
var myList : Node = null
class Node (var datum : Int, var next : Node)
{
  // (b)
  override def toString : String =
    {
      var str = ""
      var pos = myList
      // Invariant I : the string str contains numbers from the head of the list until pos.datum
      while (pos != null)
        {
          if (pos.next != null) str = str + pos.datum + " -> "
              else str = str + pos.datum // The last element doesn't have a "->"
          pos = pos.next
        }
      // I && pos = null => str contains every number from the list
      str
    }
  // >scala Question1.scala
  // List is 12 -> 11 -> 10 -> 9 -> 8 -> 7 -> 6 -> 5 -> 4 -> 3 -> 2 -> 1.
}

// (c)
def reverse =
  {
    // Reversing the order of the linked list by reversing the direction in which the list is linked
    var prev : Node = null
    var current = myList
    var next : Node = null
    // Invariant : the linked list is reversed up to prev
    while (current != null)
    {
      /* Store next node */
      next = current.next
      /* Change the direction of the current node */
      current.next = prev // the linked list is reversed up to prev.next
      /* Move prev to point to the next node */
      prev = current // the linked list is reversed up to prev && prev = current
      /* Continue the procedure for the next node */
      current = next // I
    }
    // The invariant holds => current = null and because prev.next = null,
    // the list is fully reversed, therefore we begin the list from prev:
    myList = prev
  }

def main (args: Array[String]) =
  {
    // (a) Here, we add each element to the head of myList
    for (i <- 1 to 12) myList = new Node(i,myList)
    // (c)
    //reverse
    println("List is "+myList.toString+".")
    // >scala Question1.scala
    // List is 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9 -> 10 -> 11 -> 12.
  }
}
