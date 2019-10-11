object Main {
  def main (args : Array[String]) = {
    var list = new fraction
    list.add(1)
    list.add(2)
    list.add(3)
    list.add(4)
    list.add(5)
    /*
    println(list.print)
    println(list.rat2cfRec(12,5))
    println(list.rat2cf(12,5))
    println(list.rat2cfRec(40,7))
    println(list.rat2cf(40,7))
    println(list.rat2cfRec(40,5))
    println(list.rat2cf(40,5))
    println(list.rat2cfRec(0,2))
    println(list.rat2cf(0,2))

    println(list.rat2cfRec(225,157))
    println(list.rat2cf(225,157))

    var (x,y) = list.cf2rat
    if (y == 1) println(x)
        else println(x+"/"+y)
    */
    var (a,b) = list.cf2ratRec
    if (b == 1) println(a)
        else println(a+"/"+b)
  }
}
