// Companion object
object HashBag {
  //(a)
  private class Node (var word : String, var count : Int, var next : Node)
}

// Abstraction function : table = {{n(i).word -> n(i).count | n(i) in L(table(i).next)} | i is [0..H)}
// DTI : L(table(i).next) is finite for all i in [0..H)

class HashBag{
  private def hash (word : String) : Int ={
    def f (e: Int, c : Char) = (e*41 + c.toInt) % H
    word.foldLeft(1)(f)
  }
  private val H = 100
  private var size_ = 0

  //(b)
  private val table = new Array[HashBag.Node](H)
  for (i <- 0 until H) table(i) = new HashBag.Node("?",0,null)

  private def find (w : String, head : HashBag.Node) : HashBag.Node = {
    var n = head
    while (n != null && n.word != w) n = n.next
    n
  }

  def Tally(w: String) = {
    val h = hash(w)
    val n = find(w,table(h))
    if (n != null) n.count += 1
    else {
      table(h).next = new HashBag.Node(w,1,table(h).next)
      size_ += 1
    }
  }

  //(c)
  def sortList (n : Int) : Unit = {
    var head = table(n)
    var current = head.next
    // Invariant I: the nodes up to current are sorted decreasingly
    while (current != null)
    {
      var n1 = current.next
      var prev = head
      var pos = head.next
      while (pos.count > n1.count) {prev = prev.next ; pos = pos.next}
      // Putting n1 between prev and pos
      prev.next = n1
      n1.next = pos
      // Deleting n1 from its previous position
      current.next = current.next.next
      current = current.next
    }
  }

  //(d)
  // The result will be in the first list
  def mergeList (i : Int, j : Int) : Unit = {
    var head1 = table(i)
    var head2 = table(j)
    var prev = head1
    var pos = head1.next
    var current = head2.next
    while (current != null)
    {
      // We take every node from the second list and insert it in the first one, maintaining the decreasing order, starting from where we stopped last time as the lists are already sorted
      while (pos.count > current.count) {prev = prev.next ; pos = pos.next}
      var n1 = current
      // Insert the node in the first list
      prev.next = n1
      n1.next = pos
      // Continuing the procedure for the other nodes of the second list
      current = current.next
    }
  }

  //(e)
  def sortAll = {
    var i = 0
    for (i <- 0 until H) sortList(i)
    var lists = H
    while (lists > 1)
    {
      i = 0
      var del = 0
      while (i<lists/2) {mergeList(i,lists-i-1); i += 1 ; del += 1}
      lists = lists - del
    }
  }

  //(f)

  /*
  The time complexity of sortList is O(t^2) in the worst-case scenario, when
  the frequency of the words is sorted increasingly(t=size of the list, which
  is N/H) and the time complexity of mergeList is O(t1+t2), where t1 and t2
  are the size of the first and the second lists,respectively. Therefore,
  when we add merge all the lists with the first one, we get a total of :
  2*(N/H) + 3*(N/H)+4*(N/H)+...+H*(N/H) = (N/H)*(H*(H+1)/2-1) = O(N*H) operations
  Therefore, the time complexity of sortAll is H*O(N^2/H^2) + O(N*H).
  As N/H is a constant, let's say a, the time complexity is
  O(N)+O(N*N/a)=O(N^2) so the time grows quadratically in N. The worst-case arises
  at sorting, when the lists to be sorted are already increasingly sorted.
  */
}
