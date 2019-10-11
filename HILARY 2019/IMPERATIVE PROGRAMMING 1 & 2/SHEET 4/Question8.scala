// Main module for the array bag example, using an implementation of Bag
import scala.io.StdIn.readLine

object SortingBag
{
  def main (args: Array[String]) =
    {
      val MAX = 10000
      // The array bag which we will play with, by adding elements to it and demanding the sorted array with what we have added so far
      val bag = ArrayBag
      var done = false // are we finished yet?
      while(!done)
        {
          val cmd = readLine("Add (a), copies (c), sort (s) or quit (q)? ")
          cmd match{
        case "a" => {
          val elem = (readLine("Element to add to the bag? ")).toInt
          if (elem < MAX) bag.add(elem)
          else println(elem+" exceeds the maximum value allowed.")
        }
        case "c" => {
          val elem = (readLine("What element to count? ")).toInt
          println("The element "+elem+" appears "+bag.copies(elem)+" times in the bag.")
        }
        // Here, as the question asks, we will do it in O(N+MAX)
        case "s" => {
          println("The bag contains: ")
          for (i <- 0 until MAX)
            for (j <- 0 until bag.copies(i)) print(i+" ")
          println()
          // The time complexity for this is O(N+MAX) as we go through the array from 0 until MAX and as we have N elements in the bag,
          // the sum of all the elements in the array will be equal to N, so we will print N elements in total.
        }
        case "q" => done = true
        case _ => println("Please type `a', `c', `s' or `q'.")
      }
    }
  }
}
/**
Add (a), copies (c), sort (s) or quit (q)? a
Element to add to the bag? 1
Add (a), copies (c), sort (s) or quit (q)? a
Element to add to the bag? 3
Add (a), copies (c), sort (s) or quit (q)? a
Element to add to the bag? 1
Add (a), copies (c), sort (s) or quit (q)? a
Element to add to the bag? 4
Add (a), copies (c), sort (s) or quit (q)? c
What element to count? 1
The element 1 appears 2 times in the bag.
Add (a), copies (c), sort (s) or quit (q)? c
What element to count? 5
The element 5 appears 0 times in the bag.
Add (a), copies (c), sort (s) or quit (q)? s
The bag contains:
1 1 3 4
Add (a), copies (c), sort (s) or quit (q)? a
Element to add to the bag? 20
Add (a), copies (c), sort (s) or quit (q)? a
Element to add to the bag? 20
Add (a), copies (c), sort (s) or quit (q)? a
Element to add to the bag? 15
Add (a), copies (c), sort (s) or quit (q)? s
The bag contains:
1 1 3 4 15 20 20
*/
