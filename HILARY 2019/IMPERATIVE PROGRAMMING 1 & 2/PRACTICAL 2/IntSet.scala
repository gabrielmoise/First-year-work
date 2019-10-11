// A class of objects to represent a set
/**
DESIGN DECISIONS:
1. I used a dummy header node to not bother with the special case when we remove the first node
2. My linked list doesn't have repetitions, because a mathematical set does not allow repetitions (it's not a bag)
3. I store the elements in increasing order to be able to check if two sets are equal by applying toString to both and checking for equality
between the strings.
4. For the functions size and sum I kept track of the number of elements after each function (at add and remove we update them)
Also, I created a function that transforms an increasingly-ordered list into an IntSet (so we maintain the increasing order of the data)
*/
// Abstraction function : S = {x ∈ Int} with |S| = count
// DTI : Let L(a,b) = [] if a = b and L(a,b) = a: L(a.next,b) otherwise. As an abbreviation, L(a,null) = L(a)
// Then, L(theSet.next) is finite and it has count distinct elements which are ordered in an increasing order.
class IntSet
{
  // State: S : P(Int) (where "P" represents power set)
  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  private type Node = IntSet.Node
  // Constructor
  private def Node(datum: Int, next: Node) = new IntSet.Node(datum, next)
  // Init: S = {}
  private var theSet = Node (0, null)
  var count = 0 // the number of elements from the set
  private var Sum = 0 // the sum of the elements from the set, which we'll use later
  /** Convert the set to a string.*/
  // Complexity needed : O(count)
  override def toString : String =
    {
        var str = "{" // this will contain the String to be printed
        var current : Node = theSet.next // skip the dummy header
        // Invariant : str contains the integers up to (but not including) the current node
        while (current != null)
        {
            if (current.next != null) str = str + current.datum + ", "
                else str = str + current.datum
            current = current.next
        }
        str = str + "}"
        // str contains all the integers from the list bewteen two curly brackets
        str
    }
  /** Checks if an integer e is in the set or not and returns the previous node or the last node if e is not in the set.*/
  // Complexity O(count)
  private def findElem (e: Int) : Node =
    {
        var current : Node = theSet
        while ((current.next != null) && (current.next.datum != e)) current = current.next
        current
    }
  /** Add element e to the set ; notice that we mantain the data in the set in increasing order
    * Post: S = S_0 U {e} */
  // Complexity O(count)
  def add(e: Int) =
    {
        var n = findElem(e).next
        if (n == null)
            {
            var current = theSet : Node
            // Invariant I : every node up to current.datum (we ignore the dummy header) has a value that is less than e
            while ((current.next != null) && (current.next.datum < e)) current = current.next
            // We place the node between current and current.next as all values up to current are smaller than e, whereas the data from current.next is bigger (or we reached the end of the list)
            var n1 = Node (e,current.next)
            current.next = n1
            // Updating count and Sum
            count = count + 1
            Sum = Sum + e
            }
        // otherwise the element was already in the set, so we do not add it again
    }
  /** Length of the list
    * Post: S = S_0 && returns #S */
  // O(1)
  def size : Int = count
  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S) */
  // O(n)
  def contains(e: Int) : Boolean = (findElem(e).next != null)
  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S */
  // O(1)
  def any : Int =
{
    require (theSet.next != null) // pre-condition
    theSet.next.datum // smallest element of the set
}
  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  // O(n)
   override def equals(that: Any) : Boolean = that match {
    case s: IntSet => (toString == s.toString)
    // they are equal if they have the same elements (and because they are in order, they are represented in the same way with toString)
    case _ => false
  }
  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  // O(n)
  def remove(e: Int) : Boolean =
    {
        var pos = findElem(e)
        if (pos.next == null) return false
            else {pos.next = pos.next.next ; count = count - 1; Sum = Sum - e}
        return true
    }

  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S */
  // O(|S| + |that|)
  def subsetOf(that: IntSet) : Boolean =
    {
        if (size > that.size) return false // theSet cannot be a subset of a smaller set
        var i = theSet.next
        var j = that.theSet.next
        while ((i!=null) && (j!=null))
        {
          while ((j != null) && (i.datum > j.datum)) j = j.next // As long as the element from the ith node might be in that, we keep looking
          if (j == null) return false // i.datum > last element from that, so i.datum is not in the set that
          if (i.datum < j.datum) return false // we didn't find an element in that which would be equal to i.datum and there cannot be one in the rest of the list
          if (i.datum == j.datum) {i = i.next ; j = j.next} // we found a match, so we continue
        }
        if (i != null) return false // j = null, so we haven't found all the elements from theSet in that
        // i = null so we found all elements of theSet in that
        return true
    }
  // ----- optional parts below here -----
  /** Transform an array a[0..c) into a list
      Pre : a is sorted increasingly
      Post : return res, which is an IntSet */
  // O(c)
  private def arrayToList (a : Array[Int], c : Int) : IntSet =
    {
      var res = new IntSet
      var i = c
      while (i > 0)
        {
          var n1 = new IntSet.Node (a(i-1), res.theSet.next)
          res.theSet.next = n1
          i -= 1
        }
      res
    }
  /** return union of this and that.
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
  // O(|S| + |that|)
  def union(that: IntSet) : IntSet =
    {
      var a = new Array [Int] (size+that.size)
      var current = 0 // the index of the array a
      var i = theSet.next
      var j = that.theSet.next
      // We go through the 2 lists with 2 indexes and we form the union into the res list, with current index "current"
      while ((i != null) && (j != null))
      {
        // Maintaining the increasing order of the data in a
        if (i.datum < j.datum) {a(current) = i.datum; current += 1 ; i = i.next}
        else if (i.datum == j.datum) {a(current) = i.datum; current += 1 ; i = i.next ; j = j.next}
        else if (i.datum > j.datum) {a(current) = j.datum; current += 1 ; j = j.next}
      }
      // These 2 while loops cannot both happen (at most one can happen)
      while (i != null) {a(current) = i.datum; current += 1 ; i = i.next}
      while (j != null) {a(current) = j.datum; current += 1 ; j = j.next}
      arrayToList(a,current)
    }
  /** return intersection of this and that.
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  // O(|S| + |that|)
  def intersect(that: IntSet) : IntSet =
    {
      var a = new Array [Int] (size+that.size)
      var current = 0
      var i = theSet.next
      var j = that.theSet.next
      // We go through the 2 lists with 2 indexes and we form the intersection into the res list, with current index "current"
      while ((i != null) && (j != null))
      {
        // Maintaining the increasing order of the data in res
        if (i.datum < j.datum) i = i.next // the j.datum value can appear in this afterwards
        else if (i.datum == j.datum) {a(current) = i.datum; current += 1 ; i = i.next ; j = j.next}
        else if (i.datum > j.datum) j = j.next // the i.datum value can appear in that afterwards
      }
      // If either of them is empty, we need not bother about the rest
      arrayToList(a,current)
    }
  /** map
    * Post: S = S_0 && returns res s.t. res.S = {f(x) | x <- S} */
  // We apply f to every datum field from every node, we put them into an array, we sort the array and then we put them into a res list
  // We sort them to maintain the increasing order property of the data as applying f can change that unpredictably
  // O(count*log2(count))
  def map(f: Int => Int) : IntSet =
    {
      var a = new Array [Int] (count)
      var c = 0;
      var i = theSet.next
      while (i != null) {a(c) = f(i.datum) ; c += 1 ; i = i.next}
      // The data is in a[0..count) and we want to sort it (c = count)
      var b = new Array [Int] (count)
      b = a.sorted //O(count*log2(count))
      arrayToList(b,c)
    }
  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
  // Unlike at map, we need not sort them because we either put them in res or not so as theSet is sorted, res will be too
  // O(count)
  def filter(p : Int => Boolean) : IntSet =
    {
      var a = new Array [Int] (count)
      var current = 0
      var i = theSet.next
      while (i != null)
      {
        if (p(i.datum)) {a(current) = i.datum ; current += 1}
        i = i.next
      }
      arrayToList(a,current)
    }
  // ----- more functions from the API documentation for scala.collection.mutable.Set ---
  /** takeWhile
    * Post: S = S_0 && returns res s.t. res.S = L(theSet.next) with L(null) = {} and L(x) = if (p x) x:L(x.next) else {} */
  // We take elements from the set as long as they satisfy p
  // O(count)
  def takeWhile (p : Int => Boolean) : IntSet =
    {
      var a = new Array [Int] (count)
      var current = 0
      var i = theSet.next
      while ((i != null) && (p(i.datum))) {a(current) = i.datum ; current += 1 ; i = i.next}
      arrayToList(a,current)
    }
    /** sum
      * Post: S = S_0 && returns the Sum(theSet.next) with Sum(null) = 0 and Sum(x) = x.datum + Sum(x.next) */
    // O(1)
  def sum : Int = Sum
}

// The companion object
object IntSet{
  /** The type of nodes defined in the linked list */
  private class Node(var datum: Int, var next: Node)
  /** Factory method for sets.
    * This will allow us to write, for example, IntSet(3,5,1) to
    * create a new set containing 3, 5, 1 -- once we have defined
    * the main constructor and the add operation.
    * post: returns res s.t. res.S = {x1, x2,...,xn}
    *       where xs = [x1, x2,...,xn ] */
  def apply(xs: Int*) : IntSet = {
    val s = new IntSet; for(x <- xs) s.add(x); s
  }
}
