object Question1
{
  def main(args: Array[String]) =
  {
    val myset = new scala.collection.mutable.HashSet[String]
    // myset: scala.collection.mutable.HashSet[String] = Set()
    assert (myset.size == 0)
    assert (myset.isEmpty == true)
    // In the beginning, the size of myset was 0, as expected and therefore myself is empty
    assert (myset.contains("a") == false)
    myset.add("a") ; myset.add("ab")
    assert (myset.size == 2)
    assert (myset.isEmpty == false)
    // scala.collection.mutable.HashSet[String] = Set(ab, a)
    assert (myset.contains("a") == true)
    assert (myset.contains("b") == false)
    // So, the contains function behaves as we expect
    assert (myset.add("a") == false)
    assert (myset.size == 2)
    // The first assertion is false because we cannot add "a", as it already is in the set
    assert (myset.add("b") == true)
    assert (myset.size == 3)
    // The first assertion is true because "b" is not in myset, so the function also adds "b" to myset
    // scala.collection.mutable.HashSet[String] = Set(ab, a, b)
    assert (myset.remove("c") == false)
    assert (myset.isEmpty == false)
    // This is false as we cannot remove a value which is not in myset
    assert (myset.remove("ab") == true)
    assert (myset.size == 2)
    // This is true and "ab" is removed from myset
    // scala.collection.mutable.HashSet[String] = Set(a, b)
    myset.remove("a") ; myset.remove("b")
    // scala.collection.mutable.HashSet[String] = Set()
    assert (myset.size == 0)
    assert (myset.isEmpty == true)
    // Now, a function that throws an exception is max, which returns the biggest element from the set(here in lexicografic order):
    // myset.add("a") ; myset.add("b") ; myset.add("ab") ; myset.add("ba")
    // scala> myset
    // res17: scala.collection.mutable.HashSet[String] = Set(ab, ba, a, b)
    // scala> myset.max
    // res18: String = ba
    // But when the set is empty, it throws and exception:
    // myset.remove("a") ; myset.remove("b") ; myset.remove("ab") ; myset.remove("ba")
    // scala> myset
    // res23: scala.collection.mutable.HashSet[String] = Set()
    // myset.max
    // java.lang.UnsupportedOperationException: empty.max
    // because the precondition for max is not respected, more precisely the precondition is myset.size > 0.
  }
}
