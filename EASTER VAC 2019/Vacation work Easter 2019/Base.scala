object Base {

  var base = 7

  type number = Array[Int]

  def compyNum (a : number) : number = {
    var b = new number(a.size)
    for (i <- 0 until a.size) b(i) = a(i)
    b
  }

  def lengthen (a : number) : number = {
    var N = a.size
    N = 2 * N
    var res = new number(N)
    for (i <- 0 until a.size) res(i) = a(i)
    res
  }

  def normal (a : number) : Int = {
    var N = a.size
    var baseMul = 1
    var value = 0
    for (i <- 0 until N) {value = value + a(i)*baseMul ; baseMul = baseMul * base}
    value
  }

  def makeNum (value : Int) : number = {
    if (value == 0) {var b = new number(1); b(0) = 0; return b}
    // Finding the number of digits needed for the representation in base "base" of value
    var aux = value
    var i = 0
    while (aux != 0) {aux = aux / base ; i += 1}
    var N = i
    var b = new number(N)
    i = 0
    aux = value
    while (i < N)
    {
      b(i) = aux % base
      aux = aux / base
      i += 1
    }
    b
  }

  def add (a : number, b : number) : number = {
    var result = normal(a) + normal(b)
    return makeNum(result)
  }

  def mul (a: number, b : number) : number = {
    var result = normal(a) * normal(b)
    return makeNum(result)
  }

  def outNum (a : number) : String = {
    var result = normal(a)
    return result.toString
  }

  def main (args : Array[String]) = {
    var t = new number(10)
    println(outNum(mul(makeNum(125),makeNum(200))))
  }
}
