/** Add the maplet name -> number to the mapping */

def store (name: String, number: String) =
  {
    val n = find(name)
    if (n.next == null) // store the name at the end of the list
        n.next = new Node (name, number, null)
    else n.next.number = number // modify the number associated to the found name
  }
