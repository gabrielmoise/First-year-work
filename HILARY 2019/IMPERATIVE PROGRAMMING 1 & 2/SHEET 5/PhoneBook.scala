// Main module for the phone book example, using an implementation of Book
import scala.io.StdIn.readLine
object PhoneBook{
  def main(args: Array[String]) = {
    val book = new LinkListHeaderBookOrd // the phone book object; edit to choose the implementation
    //val book = ArraysBook
    var done = false // are we finished yet?
    while(!done){
      val cmd = readLine("Order(o), Delete(d), Recall (r), store (s) or quit (q)? ")
      cmd match{
        case "o" => {
          val name = readLine("Name to find in the list? ")
          if (! book.isInBook(name)) println(name+" not found.")
          else println(book.order(name))
        }
        case "d" => {
          val name = readLine("Name to delete? ")
          if (! book.isInBook(name)) println(name+" not found.")
          else book.delete(name)
        }
        case "r" => {
          val name = readLine("Name to recall? ")
          if(book.isInBook(name)) println(book.recall(name))
          else println(name+" not found.")
        }
        case "s" => {
          val name = readLine("Name to store? ")
          val number = readLine("Number to store? ")
          book.store(name,number)
        }
        case "q" => done = true
        case _ => println("Please type `o', `d', `r', `s' or `q'.")
      } // end of match
    } // end of while
  } // end of main
}
