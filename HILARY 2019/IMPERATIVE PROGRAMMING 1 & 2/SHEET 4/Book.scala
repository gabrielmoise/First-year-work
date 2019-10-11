// The interface to the phone book

// Easch implementation of this trait represents a mapping from names
// (Strings) to numbers (also Strings).
trait Book{
  // State: book : String -|-> String
  // Init:  book = {}

  // Add the maplet name -> number to the mapping
  // POST: book = book_0 (+) {name -> number}
  def store(name:String, number:String)

  // Return the number stored against name
  // PRE: name in dom book
  // POST book = book_0 && returns book(name)
  def recall(name:String) : String

  // Is name in the book?
  // POST: book = book_0 && returns name in dom book
  def isInBook(name: String): Boolean
}
