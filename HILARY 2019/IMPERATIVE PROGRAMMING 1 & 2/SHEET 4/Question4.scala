/**
(a) First of all, by definition, a mathematical set is an UNORDERED sequence of elements, therefore the API description for
the Scala Set[A] trait is ambiguous because we can't define the "first" element of a set without having an order.
By doing some experiments I found out that it behaves in a non-intuitive way, the implementation method being not obvious at all.
For example I did :
scala> val set = scala.collection.mutable.Set[Int]()
scala> set.add(1) ; set.add(2) ; set.add(-4)
res3: scala.collection.mutable.Set[Int] = Set(1, 2, -4)
scala> set.head
res4: Int = 1 (so the head is not the smallest element)
Then I added some other numbers and it got even weirder :
scala> set.add(-1) ; set.add(-20) ; set.add(-30) ; set.add(-50)
res12: scala.collection.mutable.Set[Int] = Set(-50, 1, -20, 2, -1, -30, -4)
So it doesn't seem like there is an obvious order. But when I add 0, every time I do that, it becomes the head and it stays that way:
scala> set.add(0)
res14: scala.collection.mutable.Set[Int] = Set(0, -50, 1, -20, 2, -1, -30, -4)
scala> set.add(12) ; set.add(10) ; set.add(8)
res18: scala.collection.mutable.Set[Int] = Set(0, 12, -50, 1, -20, 2, -1, -30, 10, -4, 8)
scala> set.head
res19: Int = 0
*/
/**
(b) So, in my opinion, it would be sensible, since we only have elements in the set that are in [0..N), as in Question 3,
to define the first element of the set as the smallest one, like the set is an ordered sequence, the problem appears when
the set is empty, then an assertion is thrown in "Scala java.util.NoSuchElementException: next on empty iterator": */

// Calculating the head of the set (which we considered ordered by the "bit map" we created in Question 3)
// Pre : set is non-empty, otherwise we throw an exception
// Post : the smalles element of the set, as min{ i | 0 <= i < N such that a(i) = true}
def head : Int

// (c)

def head : Int =
  {
    var i = 0
    // Invariant I : a[0..i) = false && 0 <= i <= N
    // Variant N-i
    while (i < N && a(i) == false) i += 1
    assert (i < N) // throw an exception if the set is empty
    i // otherwise return the first i such that a(i) = true
  }
