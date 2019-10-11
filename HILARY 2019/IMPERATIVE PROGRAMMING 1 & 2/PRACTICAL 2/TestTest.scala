/**  With Scala 2.12 on Lab machines:

 * In normal circumstances the CLASSPATH is already set for you:

fsc TestTest.scala
scala org.scalatest.run TestTest

 * If you use jar files in your own space:

fsc -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar TestTest.scala
scala -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar org.scalatest.run TestTest

 * (Once this is working you can set your CLASSPATH in .bashrc)
*/
import org.scalatest.FunSuite


class TestTest extends FunSuite{
  // Some useful sets that I use for the tests
  var empty = IntSet()
  var s1 = IntSet(1,2,3)
  var s1a = IntSet(1,2,3)
  var s1r = IntSet(1,2,3)
  var s2 = IntSet(1,4,3,2,7,5)
  var s3 = IntSet(2,2,3,1)
  var s3r = IntSet(2,2,3,1)
  var big = IntSet(20,19,18,17,16,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  var s5 = IntSet(3,4,2,2,4,1,5,1,1)
  var s6 = IntSet(4,5,6)

  // CONTAINS
  test ("contains test true") {assert (s1.contains(1) === true)}
  s1a.add(4) ; s1a.add(1) ; s1a.add(1); s1r.remove(4); s1r.remove(1); s1r.remove(1);
  test ("contains test false") {assert (s1.contains(4) === false)}
  test ("contains test after add") {assert (s1a.contains(4) === true)}
  test ("contains test after remove") {assert (s1r.contains(1) === false)}
  test ("contains test empty") {for (i<-0 to 100) assert (empty.contains(i) === false)}
  // TOSTRING
  test ("toString test increasing") {assert (s1.toString === "{1, 2, 3}")}
  test ("toString test after add") {assert (s1a.toString === "{1, 2, 3, 4}")}
  test ("toString test after remove") {assert (s1r.toString === "{2, 3}")}
  test ("toString test random") {assert (s2.toString === "{1, 2, 3, 4, 5, 7}")}
  test ("toString test repetitions") {assert (s3.toString === "{1, 2, 3}")}
  test ("toString test empty") {assert (empty.toString === "{}")}
  // SIZE
  test ("size test 1") {assert (s1.size === 3)}
  test ("size test 2") {assert (big.size === 21)}
  test ("size test after add") {assert (s1a.size === 4)}
  test ("size test after remove") {assert (s1r.size === 2)}
  test ("size test empty") {assert (empty.size === 0)}
  // ADD
  // We test the addition operation throughout the test suite and check if it behaves as it should in relation to other functions, too.
  // ANY - my implementation : returns the smallest element of the set (always the first)
  test ("any test 1") {assert (s1.any === 1)}
  test ("any test 2") {assert (s2.any === 1)}
  test ("any test 3") {assert (big.any === 0)}
  test ("any test after add") {assert (s1a.any === 1)}
  test ("any test repetitions") {assert (s5.any === 1)}
  // EQUALS - works as long as TOSTRING works
  test ("equals test 1") {assert (!(s1 === s2))}
  test ("equals test 2") {assert (!(s1 === big))}
  test ("equals test after add") {assert (!(s1 === s1a))}
  test ("equals test after remove") {assert (!(s1 === s1r))}
  test ("equals test repetitions") {assert (s1 === s3)}
  test ("equals test empty") {assert (empty === empty)}
  // REMOVE
  // We test the remove operation throughout the test suite with asserts and we check if it behaves as it should in relation to other functions, too.
  test ("remove test 1") {assert (s1r.remove(4) === false)}
  test ("remove test 2") {assert (s1r.remove(2) === true)}
  test ("remove test empty") {for (i<-0 to 100) assert (empty.remove(i) === false)}
  s3r.remove(2)
  test ("remove test repetitions") {assert (s3r.remove(2) === false)}
  // SUBSETOF
  test ("subsetOf test 1") {assert (s1.subsetOf(big) === true)}
  test ("subsetOf test 2") {assert (s2.subsetOf(big) === true)}
  test ("subsetOf test after add") {assert (s1a.subsetOf(big) === true)}
  test ("subsetOf test after remove") {assert (s1r.subsetOf(big) === true)}
  test ("subsetOf test empty") {assert (s1.subsetOf(empty) === false)}
  test ("subsetOf test mirror") {assert (s1.subsetOf(s1) === true)}
  // ---------- optional ----------
  // UNION
  test ("union test 1") {assert ((s1.union(s6)).toString === "{1, 2, 3, 4, 5, 6}")}
  test ("union test 2") {assert ((s3.union(s6)).toString === "{1, 2, 3, 4, 5, 6}")}
  test ("union test repetitions") {assert ((s5.union(s6)).toString === "{1, 2, 3, 4, 5, 6}")}
  test ("union test included") {assert (s1.union(big) === big)}
  test ("union test empty") {assert (s1.union(empty) === s1)}
  test ("union test mirror") {assert (s1.union(s1) === s1)}
  test ("union test commutativity") {assert (s1.union(s6) === s6.union(s1))}
  // INTERSECT
  test ("intersect test 1") {assert ((s5.intersect(s6)).toString === "{4, 5}")}
  test ("intersect test repetitions") {assert ((s1.intersect(s2)).toString === "{1, 2, 3}")}
  test ("intersect test mirror") {assert (s1.intersect(s1) === s1)}
  test ("intersect test disjoint") {assert (s1.intersect(s6) === empty)}
  test ("intersect test included") {assert (s1.intersect(big) === s1)}
  test ("intersect test commutativity") {assert (s1.intersect(s6) === s6.intersect(s1))}
  test ("intersect test empty") {assert (s1.intersect(empty) === empty)}
  // MAP
  var set1 = IntSet(1,2,3)
  var set2 = IntSet(5,4,2,3)
  var set3 = IntSet(0,10,20,30)
  var set4 = IntSet(1,2,3,2,1,1)
  test ("map test 1") {assert (((set1.map((x: Int) => 2*x)).toString) === "{2, 4, 6}")}
  test ("map test 2") {assert (((set2.map((x: Int) => 2*x)).toString) === "{4, 6, 8, 10}")}
  test ("map test 3") {assert (((set3.map((x: Int) => x/10)).toString) === "{0, 1, 2, 3}")}
  test ("map test repetitions") {assert (((set4.map((x: Int) => 2*x)).toString) === "{2, 4, 6}")}
  test ("map test empty") {assert (((empty.map((x: Int) => 2*x)).toString) === "{}")}
  test ("map test inverse") {assert (((set1.map((x: Int) => 5-x)).toString) === "{2, 3, 4}")}
  //FILTER
  test ("filter test 1") {assert (((set1.filter((x: Int) => x%2==0)).toString) === "{2}")}
  test ("filter test 2") {assert (((set3.filter((x: Int) => x%10==0)).toString) === "{0, 10, 20, 30}")}
  test ("filter test repetitions") {assert (((set4.filter((x: Int) => x%2==0)).toString) === "{2}")}
  test ("filter test empty") {assert (((empty.filter((x: Int) => x > 0)).toString) === "{}")}
  // ---------- very optional ----------
  // TAKEWHILE
  test ("takeWhile test 1") {assert (((set1.takeWhile((x: Int) => x%2==0)).toString) === "{}")}
  test ("takeWhile test 2") {assert (((set3.takeWhile((x: Int) => x%10==0)).toString) === "{0, 10, 20, 30}")}
  test ("takeWhile test repetitions") {assert (((set4.takeWhile((x: Int) => x<2)).toString) === "{1}")}
  test ("takeWhile test empty") {assert (((empty.takeWhile((x: Int) => x > 0)).toString) === "{}")}
  // SUM
  test ("sum test 1") {assert (set1.sum === 6)}
  test ("sum test 2") {assert (set2.sum === 14)}
  test ("sum test after add") {assert (s1a.sum === 10)}
  test ("sum test after remove") {assert (s1r.sum === 3)} // we removed 1 at start and then 2 in an assertion
  test ("sum test repetitions") {assert (set4.sum === 6)}
  test ("sum test empty") {assert (empty.sum === 0)}
}
