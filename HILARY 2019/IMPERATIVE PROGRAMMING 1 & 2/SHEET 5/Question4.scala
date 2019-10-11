/*
(a)
The expected amount of work done by a recall function is E, given by the
formula sum from i=0 to (n-1) of work(i) * p(i), where work(i) is the number
of operations needed to reach the ith node of the linked list, which, in our
case of a linear algorithm of searching, will be (i+1) and p(i) is the
probability that the ith name would be recalled. Also, we have to add to E
the work needed in the case when we recall a name that doesn't exist in the
list, and that is w(none) = n and the probability to recall such a name,
q = 1 - (p(0) + p(1) + ... + p(n-1))
Therefore, we have E = p(0) + 2*p(1) + 3*p(2) + ... + (n-1)*p(n-2) + n*p(n-1) + n*q,
which would obviously be minimized when p(0)>=p(1)>= ... >=p(n-1).
*/

// (b)
// The interface to the phone book

// When a name is recalled, we search for it, and then
// we save its data separately, create a new node that will be put at
// the head of the list, and then the node where we found the name will be
// deleted.

// Abstraction function : book = {(n.name -> n.number) | n is in L(list.next)}
// where L(a,b) = [] if a=b and L(a,b) = a : L(a.next,b), otherwise. Also,
// L(a) = L(a,null) as an abreviation (from the lecture)
// DTI : L(list.next) is finite, the names are distinct and sorted according to the "most recently used" rule (the last recalled is at the head of the list)

class LinkedListProbabilityBook extends Book{
  private var list = new LinkedListProbabilityBook.Node("?", "?", null)

  private def find(name:String) : LinkedListProbabilityBook.Node = {
    var n = list
    while(n.next != null && n.next.name != name) n = n.next
    n
  }

  def isInBook(name: String): Boolean = find(name).next != null

  // When we recall name, we move the node which contains it to the head of the list
  def recall(name: String) : String = {
    val n = find(name);
    require (n.next != null)
    // Preserving the recalled number
    val number = n.next.number
    // Deleting the node from the current position
    n.next = n.next.next
    // Adding the node to the head of the list
    list.name = name; list.number = number
    list = new LinkedListProbabilityBook.Node("?", "?", list)
    // Returning the desired number
    return number
  }

  /** Add the maplet name -> number to the mapping */
  def store(name: String, number: String) = {
    val n = find(name)
    if(n.next == null){ // store new info in current list header
      list.name = name; list.number = number
      list = new LinkedListProbabilityBook.Node("?", "?", list)
    }
    else n.next.number = number
  }

  /** Delete the number stored against name (if it exists);
    * return true if the name existed. */
  def delete(name: String) : Boolean = {
    val n = find(name)
    if(n.next != null){ n.next = n.next.next; true }
    else false
  }
}

// Companion object
object LinkedListProbabilityBook{
  private class Node(var name:String, var number:String, var next:Node)
}
