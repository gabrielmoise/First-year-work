class fraction {
  private var list = new fraction.Node (0,null)
  private var end = list

  def add (d : Int) : Unit = {
    var n1 = new fraction.Node(d,null)
    end.next = n1
    end = n1
  }

  def print : String = {
    var current = list.next
    var par = 0
    var str = ""
    while (current != null)
    {
      if (current.next == null) str = str + current.datum
          else if (current.next.next == null) str = str + current.datum+"+1/"
          else {str = str + current.datum+"+1/(" ; par += 1}
      current = current.next
    }
    for (i <- 0 until par) str = str + ")"
    str
  }

  def rat2cfRec (n : Int, d : Int) : String = {
    if (n%d == 0) return (n/d).toString
    else if (n%d == 1) return (n/d).toString + "+1/" + rat2cfRec(d,n%d)
    else return (n/d).toString + "+1/(" + rat2cfRec(d,n%d) + ")"
  }

  def rat2cf (n1 : Int, d1 : Int) : String = {
    var frac = new fraction
    var n = n1
    var d = d1
    while (d != 0)
    {
      var a = n / d
      var b = n % d
      frac.add(a)
      n = d
      d = b
    }
    frac.print
  }

  def del : Unit = list.next = list.next.next

  def cf2ratRec : (Int,Int) = {
    var x = list.next.datum
    if (list.next.next == null) return (x,1)
    else
    {
      this.del
      var (n1,d1) = this.cf2ratRec
      return (n1*x+d1,n1)
    }
  }

  def cf2rat : (Int,Int) = {
    var current = list.next
    var (m1,m2,m3,m4) = (current.datum,1,1,0)
    var n = 0
    var d = 0
    if (current.next == null) return (current.datum,1) //the result is an integer
        else current = current.next
    while (current != null)
    {
      if (current.next == null) {n = current.datum; d = 1}
          else
          {
            var p1 = m1*current.datum + m2
            var p2 = m1
            var p3 = m3*current.datum + m4
            var p4 = m3
            m1 = p1 ; m2 = p2 ; m3 = p3 ; m4 = p4
          }
      current = current.next
    }
    return (m1*n+m2*d,m3*n+m4*d)
  }
}

// Companion object
object fraction {
  private class Node (var datum : Int, var next : Node)
}
