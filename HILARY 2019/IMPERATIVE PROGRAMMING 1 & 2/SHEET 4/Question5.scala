/** The state of a phone book, mapping names (Strings) to numbers (also
  * Strings).
  * state: book : String→String
  * init: book = {} */
trait Book
{
  /** Add the maplet name -> number to the mapping.
    * post: book = book0 ⊕{name → number} */
    def store(name: String, number: String)

    /** Return the number stored against name.
      * pre: name ∈ dombook
      * post: book = book0 ∧ returns book(name) */
    def recall(name: String) : String

    /** Is name in the book?
      * post: book = book0 ∧ returns name ∈ dombook */
    def isInBook(name: String) : Boolean

    /** Delete the number stored against name (if it exists)
      * (a) post : book = book0 - {name -> number} (if (name ∈ dombook)) || book = book0 (if  otherwise) && returns (name ∈ dombook) */
    def delete(name: String) : Boolean
}
