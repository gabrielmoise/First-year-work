/** Import is for readLine so that we can write input directly to the program */
import scala.io.StdIn
object Brack{
 //Maximum length of word so we can define our arrays in dynamic programming
 val MAXWORD = 30
 //Operation to take 'A', 'B' and 'C' to corresponding Ints
  def LetterToInt(a: Char) : Int = {
  if(a == 'A' || a == 'B' || a == 'C'){
   return (a.toInt - 'A'.toInt);
  } else{
   println("Please only Letters from A,B,C.")
   sys.exit
  }
 }
  //Defining the op array for everything to use
  val op = Array.ofDim[Int](3,3)
 op(0)(0) = 1; op(0)(1) = 1; op(0)(2) = 0
 op(1)(0) = 2; op(1)(1) = 1; op(1)(2) = 0
 op(2)(0) = 0; op(2)(1) = 2; op(2)(2) = 2
  /** Read file into array (discarding the EOF character) */
  def readFile(fname: String) : Array[Char] =
    scala.io.Source.fromFile(fname).toArray.init
  /* Functions below here need to be implemented */
 //TASK 1
 //PossibleRec checks whether bracketing to something is possible recursively
 //Checks whether w[i..j) can be bracketed to obtain z
 def PossibleRec1(w: Array[Int], i: Int, j: Int, z: Int): Boolean =
  {
    // Base case
    if (i+1==j) return w(i) == z // w[i..i+1) = w(i)
    // j >= i+2
   // We split the word into 2 subwords w[i..j) = w[i..k) && w[k..j) and we recurse on them
    for (k<- i+1 until j; z1 <- 0 to 2; z2 <- 0 to 2)
 if (op(z1)(z2)==z && PossibleRec1(w,i,k,z1) && PossibleRec1(w,k,j,z2)) return true
    return false
 }
 //TASK 2
 //NumberRec which checks the ways you get a result recursively
 //Computes number of ways w(i,j) can be bracketed to get z
 def NumberRec1(w: Array[Int], i: Int, j: Int, z: Int): Int =
  {
    // By splitting the problem into two subproblems, to get the result we need to multiply the ways
    // of bracketing the left part into what we need with the ways of bracketing the right part into what we need
    var sum = 0
    if (i+1 == j)
    {
      if (w(i) == z) return 1 // One way of bracketing here
                else return 0 // No way of bracketing here
    }
    // We sum over all possible ways of bracketing, same recursive reasoning as for PossibleRec
    for (k <- i+1 until j; z1 <- 0 to 2; z2 <- 0 to 2)
 if (op(z1)(z2)==z) sum = sum + NumberRec1(w,i,k,z1)*NumberRec1(w,k,j,z2)
    sum
 }
 //TASK 3
 //TODO Runtime analysis of recursive solution along with tests
 /** Let's analyse the time complexity of the PossibleRec and NumberRec functions (they behave the same in the worst-case scenario)
 We concentrate on the for loop, as that's where the most action happens:
 Let T(i,j) = the number of operations needed for the for loop for PossibleRec/NumberRec (i,j). Then, we get the following recurrence relation:
 T(i,j) = 3*(Sum from k=(i+1) to (j-1) of (T(i,k) + T(k,j) + 2)) +(j-i), the 2 comes from the multiplication needed and the addition to sum and the
 3 comes from the fact that we can only have op(z1)(z2) == z only 3 times from the table of the op operation. The (j-i) comes from the for loop
 Now, if we suppose that T(i,j) is exponential, let's say T(a,b) = Ω(x^(b-a)), we get:
 T(i,j) >= 3*(Sum from k=(i+1) to (j-1) of (x^(k-i) + x^(j-k) + 2)) (As here we don't count the (j-i) term)
        >= 3*((x^1 + x^2 + ... + x^(j-i-1)) + (x^(j-i-1) + x^(j-i-2) + ... + x^1) + 2)
        >= 3*2*(x^0 + x^1 + ... + x^(j-i-1))
        >= 6*(x^(j-i) - 1) / (x - 1)
  We can see that if we take x to be 7, we obtain:
  T(i,j) >= 6*(7^(j-i) - 1) / 6 = 7^(j-i) - 1 => T(i,j) = Ω(7(j-i)), so we can conclude that the time complexity of the two functions is lower
  bounded by Ω(7^n). The (j-i) term should not be crucial here, so we can deduce that this lower bound is tight, as we will see from the tests
  for big lengths of the words:
1.
Bracketing values for ABBA
A can be achieved in 2 ways
B can be achieved in 1 way
C can be achieved in 2 ways
user    0m0.782s
2.
Bracketing values for ABBAB
A can be achieved in 2 ways
B can be achieved in 10 ways
C can be achieved in 2 ways
user    0m0.802s
3.
Bracketing values for BCAABC
A can be achieved in 22 ways
B can be achieved in 9 ways
C can be achieved in 11 ways
user    0m0.826s
4.
Bracketing values for BCAABCC
A can be achieved in 71 ways
B can be achieved in 22 ways
C can be achieved in 39 ways
user    0m1.012s
5.
Bracketing values for BCAABCCB
A can be achieved in 141 ways
B can be achieved in 190 ways
C can be achieved in 98 ways
user    0m1.073s
6.
Bracketing values for BCAABCCBA
A can be achieved in 539 ways
B can be achieved in 359 ways
C can be achieved in 532 ways
user    0m2.009s
7.
Bracketing values for BCAABCCBAA
A can be achieved in 1517 ways
B can be achieved in 2126 ways
C can be achieved in 1219 ways
user    0m5.159s
8.
Bracketing values for BCAABCCBAAA <- length = 11
A can be achieved in 5425 ways
B can be achieved in 5999 ways
C can be achieved in 5372 ways
user    0m27.294s
9.
Bracketing values for BCAABCCBAAAC <- length = 12
A can be achieved in 26616 ways
B can be achieved in 12656 ways
C can be achieved in 19514 ways
user    3m11.044s <- it is approximately 7 times bigger than for the previous test


After test 5 (length = 8) we begin to see a very big change in the needed time to calculate the number of ways of obtaining each letter, one which can be supposed to be exponential, and by solving the recurrence relation of the time complexity for length N it becomes clear.

*/
 //You may find the following class useful for Task 7
 // Binary tree class
 abstract class BinaryTree
 case class Node (left : BinaryTree, right : BinaryTree) extends BinaryTree
 case class Leaf (value : Char) extends BinaryTree
 //Printing for a binary tree
 def print_tree(t : BinaryTree): Unit = t match
 {
   case s : Node =>
   {
     print("(")
     print_tree(s.left)
     print_tree(s.right)
     print(")")
   }
   case s : Leaf => print(s.value)
 }
 //These arrays should hold the relevant data for dynamic programming
 var poss = Array.ofDim[Boolean](MAXWORD, MAXWORD, 3)
 var ways = Array.ofDim[Int](MAXWORD, MAXWORD, 3)
 var exp = Array.ofDim[BinaryTree](MAXWORD, MAXWORD, 3)
 //Task 4, 5, and 7(optional)
 //TODO Fill out arrays with dynamic programming solution
// Task 4
def Tabulate(w: Array[Int], n: Int): Unit =
  {
    // Filling poss
    // First, every word of length 1 is treated as a separate case
    for (i <- 0 to n; j <- 0 to n; z <- 0 to 2) poss(i)(j)(z) = false
    for (i <- 0 until n; z <- 0 to 2) poss(i)(i+1)(z) = (w(i) == z)
    for (d <- 2 to n) // the distance between the first and the last letter of the subword -> O(N)
      for (i <- 0 to n-d) // for every first letter that we can have -> O(N)
         for (k <- i+1 until i+d) // for every place where we split de subword -> O(N)
            for (z <- 0 to 2 ; z1 <- 0 to 2; z2 <- 0 to 2) // for every combination of letters -> O(1)
                if ((op(z1)(z2) == z) && poss(i)(k)(z1) && poss(k)(i+d)(z2)) poss(i)(i+d)(z) = true
    // The complexity for this is O(N^3)
// Task 5
    // Filling ways
    // First, every word of length 1 is treated as a separate case
    for (i <- 0 to n; j <- 0 to n; z <- 0 to 2) ways(i)(j)(z) = 0
    for (i <- 0 until n; z <- 0 to 2) if (w(i) == z) ways(i)(i+1)(z) = 1
    for (d <- 2 to n) // the distance between the first and the last letter of the subword -> O(N)
      for (i <- 0 to n-d) // for every first letter that we can have -> O(N)
         for (k <- i+1 until i+d) // for every place where we split de subword -> O(N)
            for (z <- 0 to 2 ; z1 <- 0 to 2; z2 <- 0 to 2) // for every combination of letters -> O(1)
                if (op(z1)(z2) == z) ways(i)(i+d)(z) += ways(i)(k)(z1)*ways(k)(i+d)(z2)
    // The complexity for this is O(N^3)
    // Filling exp
    for (i <- 0 until n; z <- 0 to 2) if (w(i) == z) exp(i)(i+1)(z) = new Leaf (('A'.toInt+z).toChar)
    for (d <- 2 to n)
      for (i <- 0 to n-d)
         for (k <- i+1 until i+d)
            for (z <- 0 to 2 ; z1 <- 0 to 2; z2 <- 0 to 2)
                if ((op(z1)(z2) == z) && poss(i)(k)(z1) && poss(k)(i+d)(z2))
          exp(i)(i+d)(z) = new Node (exp(i)(k)(z1), exp(k)(i+d)(z2))
    // The complexity for this is O(N^3)
 }
def PossibleRec(w: Array[Int], i: Int, j: Int, z: Int): Boolean =
  {
    Tabulate(w,j)
    poss(i)(j)(z)
  }
def NumberRec(w: Array[Int], i: Int, j: Int, z: Int): Int =
  {
    Tabulate(w,j)
    ways(i)(j)(z)
  }
 //Task 6
 //TODO Runtime analysis of dynamic programming version with tests
 /**
 We can observe that every possible bracketing has a result which is either A, B or C. So, we have the total number of
 bracketings of A + those of B + those of C = total number of bracketings of the initial expression of length n. Thus, we have
 Br(A) + Br(B) + Br(C) = C(n-1), where Br(A) = total number of bracketings to obtain A, and C(n) = the nth Catalan number
 As we want to determine the biggest n for which the program can correctly determine the number of bracketings, we have to determine
 the maximum n for which C(n-1) does not exceed Int.MaxValue, which is 2,147,483,647, because in the worst-case scenario, when the
 initial text to be bracketed is formed only of Bs, the result will always be B, so Br(B) = C(n-1), and we want it to be an Int.
 As C(19) = 1,767,263,190 < Int.MaxValue and C(20) = 6564120420 > Int.MaxValue we can have n = 20 the maximum word length
 >scala Brack -NumberRec
 BBBBBBBBBBBBBBBBBBBB  -> 20 Bs
 Bracketing values for BBBBBBBBBBBBBBBBBBBB
 A can be achieved in 0 ways
 B can be achieved in 1767263190 ways
 C can be achieved in 0 ways
 >scala Brack -NumberRec
 BBBBBBBBBBBBBBBBBBBBB -> 21 Bs
 Bracketing values for BBBBBBBBBBBBBBBBBBBBB
 A can be achieved in 0 ways
 B can be achieved in -2025814172 ways -> overflow
 C can be achieved in 0 ways
 As we explained in the tabulate function, the time complexity for PossibleRec and NumberRec is O(N^3), where N is the length of the string we want to bracket. This method is definitely better for any length in case of running time, but it uses O(N^2) memory space whereas the recursive version needs a lot of recursive calls, so it also uses a lot of space of the stack.
 Some tests:
1.
Bracketing values for BCAABCCBAA <-length = 10 (compared to 5 seconds)
A can be achieved in 1517 ways
B can be achieved in 2126 ways
C can be achieved in 1219 ways
user    0m0.857s
2.
Bracketing values for BCAABCCBAAAC <-length=12(compared to 3 minutes for the recursive version)
A can be achieved in 26616 ways
B can be achieved in 12656 ways
C can be achieved in 19514 ways
user    0m0.871s
3.
Bracketing values for BCAABCCBAAACABCA <- length = 16
A can be achieved in 3156310 ways
B can be achieved in 3643843 ways
C can be achieved in 2894692 ways
user    0m0.933s
4.
Bracketing values for BCAABCCBAAACABCAABCABCABCABBA
A is Possible
B is Possible
C is Possible
user    0m0.993s
5.
Bracketing values for BBBBBBBBBBBBBBBBBBBBBBBBBBBBB
A is not Possible
B is Possible
C is not Possible
user    0m1.036s
 We can see clearly that the dynamic programming approach is faster than the recursive one.

*/
/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {
    // string to print if error occurs
    val errString =
      "Usage: scala Brack -PossibleRec [file]\n"+
      "     | scala Brack -NumberRec [file]\n"+
      "     | scala Brack -Tabulate [file]\n"
  if (args.length > 2){
   println(errString)
   sys.exit
  }
    //Get the plaintext, either from the file whose name appears in position
    //pos, or from standard input
    def getPlain(pos: Int) =
      if(args.length==pos+1) readFile(args(pos)) else StdIn.readLine.toArray
    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}
    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
  val plain = getPlain(1)
    val command = args(0)
  //Making sure the letters are of the right type
  val len = plain.length
  var plainInt = new Array[Int](len)
  if (len > MAXWORD){
   println("Word Too Long! Change MAXWORD")
   sys.exit;
  } else {
     for (i <- 0 until len){
    plainInt(i) = LetterToInt(plain(i))
   }
  }
  //Executing appropriate command
    if(command== "-PossibleRec" ){
  println("Bracketing values for " + plain.mkString(""))
  for(i<-0 to 2){
   if(PossibleRec(plainInt, 0, len, i)){
    println(('A'.toInt + i).toChar + " is Possible");
   }
   else{
    println(('A'.toInt + i).toChar + " is not Possible");
   }
  }
    }
    else if(command== "-NumberRec"){
  var z: Int = 0
  println("Bracketing values for "+ plain.mkString(""))
  for(i<-0 to 2){
   z = NumberRec(plainInt, 0, len, i)
   if(z == 1){
    printf(('A'.toInt + i).toChar+ " can be achieved in %d way\n", z)
   }
   else{
    printf(('A'.toInt + i).toChar+ " can be achieved in %d ways\n", z)
   }
  }
    }
    else if(command== "-Tabulate"){
  Tabulate(plainInt,len)
  println("Bracketing values for "+ plain.mkString(""))
  for(v<-0 to 2){
  var z: Int = ways(0)(len)(v)
   if(z==0){
   println(('A'.toInt + v).toChar+ " cannot be achieved")
   }
   else if(z==1){
    printf(('A'.toInt + v).toChar+ " can be achieved %d way\n", z)
    printf("For example:")
    print_tree(exp(0)(len)(v))
    printf("\n")
   }
   else if (z > 1){
    printf(('A'.toInt + v).toChar+ " can be achieved %d ways\n", z)
    printf("For example:")
    print_tree(exp(0)(len)(v))
    printf("\n")
   }
  }
    }
    else println(errString)
  }
}
