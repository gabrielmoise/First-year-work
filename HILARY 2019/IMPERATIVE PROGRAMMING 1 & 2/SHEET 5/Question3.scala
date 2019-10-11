// Representing the phone book using a linked list with a dummy header and keeping
// the names in alphabetical order

// Abstraction function : book = {(n.name -> n.number) | n is in L(list.next)}
// where L(a,b) = [] if a=b and L(a,b) = a : L(a.next,b), otherwise. Also,
// L(a) = L(a,null) as an abreviation (from the lecture)
// DTI : L(list.next) is finite, and the names are distinct and sorted alphabetically
class LinkListHeaderBookOrd extends Book
{

    private class Node (var name : String, var number : String, var next: Node)
    {
      override def toString : String =
        {
          var str = ""
          var pos = list
          while (pos != null)
            {
              if (pos.next != null) str = str + pos.name + " -> "
                  else str = str + pos.name
              pos = pos.next
            }
          str
        }
    }

    private var list = new Node ("?" , "?" , null)
    // list represents the mapping composed of (n.name -> n.number) maplets,
    // when n is a node reached by following 1 or more next references and
    // the names in list are sorted alphabetically.

    /** Return the node before the one containing name.
      * Post: book = book_0 && returns n s.t. n in L(list) &&
      * (n.next.name=name or n.next=null if no such Node exists)*/
    // Since we cannot use binary search on a linked list (we can, but it is
    // slightly more inefficient than the usual finding method in O(n)), we will
    // stick to a usual linear search
    private def find (name: String) : Node =
      {
        var n = list
        // Invariant: name does not appear in the nodes up to and including n; we suppose that "?" will never be introduced as a name in the phone book
        // i.e., for all n1 in L(list.next, n.next), n1.name != name
        while (n.next != null && n.next.name != name) n = n.next
        n
      }

      /** Is name in the book?
        * Post: book = book_0 && returns if we found n such that n.next.name = name */
      def isInBook(name: String): Boolean = find(name).next != null

      /** Return the number stored against name */
      def recall(name: String) : String = {
        val n = find(name); assert(n.next != null); n.next.number
      }

      // Note that the only difference appears at the store function

      /** Add the maplet name -> number to the mapping mantaining the
          alphabetical order */
      def store (name : String, number : String) =
        {
          val n = find(name) // We have n.next.name = name or n.next = null
          // If the name we want to add is not in the list, we must add it
          // in the correct place to maintain the DTI
          if (n.next == null)
          {
            // We will search for the position of where the name should be put
            // so that we maintain the DTI
            var prev = list
            var current = list.next
            // We will consider that "?" is smaller than any name we would want to add
            // Invariant I : name is bigger than every name up to, but not including current.name
            // && current = prev.next
            while ((current != null) && (name > current.name))
            {
              prev = prev.next
              current = current.next
            }
            // From the invariant, we know that name is bigger than every name up to,
            // but not including the current node, so we should put the name in a node
            // that will be introduced between prev and current
            var n1 = new Node (name, number, current)
            prev.next = n1
          }
          else n.next.number = number
      // Finding the node that have node.next.name = name and then skiping node.next
      def delete (name : String) : Boolean =
        {
          val n = find(name)
          if (n.next != null) {n.next = n.next.next ; true}
          else false
        }
      def order (name : String): Int =
        {
          var i = 0
          var current = list
          while (current.next.name != name) {i = i + 1 ; current = current.next}
          i
        }
}
