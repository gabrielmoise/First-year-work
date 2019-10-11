// Representing the phone book using a wrapper around a map
object MapBook extends Book{
//class MapBook extends Book{
  private val map = scala.collection.mutable.Map[String,String]()

  // Add the maplet name -> number to the mapping
  def store(name:String, number:String) = map.update(name,number)

  // Return the number stored against name
  def recall(name:String) : String = {
    assert(map.contains(name))
    map(name)
  }

  // Is name in the book?
  def isInBook(name: String): Boolean = map.contains(name)


}
