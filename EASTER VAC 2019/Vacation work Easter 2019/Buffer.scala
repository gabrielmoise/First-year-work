object Buffer{
  def main (args: Array[String]) = {
    var list = new LLBuffer

    list.append(2)
    list.prepend(1)
    list.append(3)
    list.prepend(0)
    var res = list.get(2)
    list.rev

    println(res)
    println(list.get(0)+" "+list.get(0)+" "+list.get(0))
  }
}
