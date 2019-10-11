class CountTree {
  class Tree (var word : String, var count : Int, var left : Tree, var right : Tree)

  private var root : Tree = null

  // DTI : Let B(t) = B(t.left) ++ [t.word] ++ B(t.right) && B(null) = []
  // Then, we have B(t.left) < t.word < B(t.right) && t.count > 0 for all t

  def add(word : String) : Unit = root = addToTree(word,root)

  def addToTree(word : String, t : Tree) : Tree = {
    var current = t
    if (current == null) current = new Tree (word,1,null,null)
    else if (current.word > word) current.left = addToTree(word,current.left)
    else if (current.word < word) current.right = addToTree(word,current.right)
    else current.count += 1
    current
  }

  // DTI : Let L(a,b) = if (a!=b) a : L(a.next,b) else []. Then,
  // L(start,end) is sorted alphabetically and is finite.

  def displayList : Unit = {
    var pair = flattenIter(root)
    var start = pair._1
    var end = pair._2
    var current = start
    while (current != end)
    {
      println(current.word+" : "+current.count)
      current = current.right
    }
    println(end.word+" : "+end.count)
  }

  def flatten (t: Tree) : (Tree,Tree) = {
    if (t == null) return (null,null)
    else if ((t.left == null) && (t.right == null)) return (t,t)
    else if (t.left == null)
    {
      var (a,b) = flatten(t.right)
      a.left = t
      t.right = a
      return (t,b)
    }
    else if (t.right == null)
    {
      var (a,b) = flatten(t.left)
      b.right = t
      t.left = b
      return (a,t)
    }
    else
    {
      var (x,a) = flatten(t.left)
      var (b,y) = flatten(t.right)
      a.right = t
      b.left = t
      t.left = a
      t.right = b
      return (x,y)
    }
  }

  def flattenIter (t : Tree) : (Tree,Tree) = {
    val stack = new scala.collection.mutable.Stack[Tree]
    var current = t
    var start : Tree = null
    var end : Tree = null
    while ((current != null) || (! stack.isEmpty))
    {
      if (current != null) {stack.push(current) ; current = current.left}
      else
      {
        current = stack.pop
        if (start == null)
        {
          start = current
          end = current
        }
        else
        {
          end.right = current
          current.left = end
          end = current
        }
        current = current.right
      }
    }
    (start,end)
  }

  def display : Unit = printTree(root)

  def printTree (t : Tree) : Unit = {
    if (t != null)
    {
      printTree(t.left)
      println(t.word+" : "+t.count)
      printTree(t.right)
    }
  }
}
