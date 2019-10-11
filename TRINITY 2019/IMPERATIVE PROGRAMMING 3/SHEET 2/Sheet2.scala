// Question 1

class FilterIterator[T] (test : T => Boolean, it : Iterator[T]) extends Iterator[T] {
  private var _next : Option[T] = None
  advance

  /** Advances the iterator until we get to an element that satisfies test, or not
    * if _next = None, there is not next element found
    * otherwise _next = Some(element) */

  private def advance () = {
    _next = None
    var found = false
    while (it.hasNext && ! found)
    {
      var current = it.next
      if (test(current)) {_next = Some(current) ; found = true}
    }
  }

  override def hasNext () : Boolean =
    {
      _next match {
        case None => false
        case _    => true
      }
    }

  override def next() : T =
    {
      _next match {
        case None => throw new NoSuchElementException("There is no next element")
        case Some(x) => {advance ; return x} // we only advance at a next instruction
      }
    }
}

// Question 2

trait Command[T] {
  def execute(target: T): Option[Change]
}

trait Change {
  def undo()
}

trait Account {
  private var money = 0

  // Abs : money is a non-negative integer that reflects the balance of the
  //       current account

  /** deposit a non-negative amount of money from the account*/
  // Pre : x >= 0
  // Post : money = money + x && return (x>=0)
  def deposit (x : Int) : Boolean

  /** withdraw a non-negative amount of money from the account*/
  // Pre : x > 0 && x <= money
  // Post : money = money - x && return (x>=0) && (money >= x)
  def withdraw (x : Int) : Boolean

  /** see the balance of the account */
  // Post : return money
  def balance : Int
}

class DepositCommand (amount : Int) extends Command[Account] {
  override def execute (target : Account) : Option[Change] =
    {
      if (target.deposit(amount))
        Some (new Change {def undo = target.withdraw(amount)} )
      else None
    }
}

class WithdrawCommand (amount : Int) extends Command[Account] {
  override def execute (target : Account) : Option[Change] =
    {
      if (target.withdraw(amount))
        Some (new Change {def undo = target.deposit(amount)} )
      else None
    }
}

class BasicAccount (_balance : Int) extends Account {
  private var money = _balance

  def deposit (x : Int) : Boolean =
    {
      if (x >= 0) {money += x ; return true}
      else return false
    }

  def withdraw (x : Int) : Boolean =
    {
      if ((x >= 0) && (money >= x)) {money -= x ; return true}
      else return false
    }

  def balance : Int = money
}

// Question 3

trait PriorityQueue {
  def isEmpty: Boolean // Determine if queue is empty
  def insert(e:Int) // Place the element e in the queue
  def remove(e:Int) // Remove (one copy of) element e (if present)
                    // ...(this operation is needed to undo insert)
  def delMin(): Int // Remove and return the smallest element
}

class InsertCommand (e : Int) extends Command[PriorityQueue] {
  override def execute (target : PriorityQueue) : Option[Change] =
  {
    target.insert(e)
    Some(new Change {def undo = target.remove(e)})
  }
}

class DelMinCommand extends Command[PriorityQueue] {
  override def execute (target : PriorityQueue) : Option[Change] =
  {
    if (target.isEmpty) return None
    var min = target.delMin
    Some(new Change {def undo = target.insert(min)})
  }
}

// Question 4

class AndThenCommand[T] (first : Command[T], second : Command[T]) extends Command[T] {
  override def execute (target : T) : Option[Change] =
  {
    var ch1 = new Change {def undo = {}}
    first.execute(target) match {
      case None => None
      case (Some(ch)) => ch1 = new Change {def undo = ch.undo}
    }

    second.execute(target) match {
      case None => {ch1.undo ; None}
      case (Some (ch2)) => Some(new Change {def undo = {ch2.undo ; ch1.undo}})
    }
  }
}

// Question 5

class WhileCommand[T](test:T => Boolean, cmd:Command[T]) extends Command[T] {
  override def execute (target : T) : Option[Change] = {
    var change = new Change {def undo = {}}
    if (! test(target)) return Some(change)
    cmd.execute(target) match {
      case None => return None
      case Some(ch) => change = new Change {def undo = ch.undo}
    }
    var nrUndo = 1 // how many times we need to apply cmd.undo to undo the whole thing
    while (test(target))
    {
      cmd.execute(target) match {
        case None => {for (i <- 0 until nrUndo) change.undo ; return None}
        case Some(ch) => nrUndo += 1
      }
    }
    Some(new Change {def undo = {for (i <- 0 until nrUndo) change.undo}})
  }
}

object Tester {
  def makeTransaction[T] (commands : List[Command[T]]) : Command[T] =
    new Command[T]
    {
      def execute (target : T) : Option[Change] =
      {
        if (commands.isEmpty) Some (new Change {def undo = {}})
        var list = commands
        var listChanges = List[Change]()
        while (! list.isEmpty)
        {
          var current = list.head
          current.execute(target) match {
            case None =>
            {
              while (! listChanges.isEmpty)
              {
                var currentChange = listChanges.head
                currentChange.undo
                listChanges = listChanges.tail
              }
              return None
            }
            case Some(ch) => listChanges = ch :: listChanges
          }
          list = list.tail
        }
        var change = new Change
        {
          def undo =
            {
              while (! listChanges.isEmpty)
              {
                var currentChange = listChanges.head
                currentChange.undo
                listChanges = listChanges.tail
              }
            }
        }
        return Some(change)
      }
    }

  def treshold (limit : Int) : (PriorityQueue => Boolean) = {
    (queue  => testQueue(queue,limit))
  }

  def testQueue (queue : PriorityQueue, limit : Int) : Boolean = {
    var min = queue.delMin
    if (min < limit) return true
    queue.insert(min)
    return false
  }

  def main(args : Array[String]) = {
    /** Tests for Question 1 */
    println("-----------------------")
    println("Printing for Question 1")
    println("-----------------------")
    // Test 1.1 - normal situation
    var string1 = "Gabriel"
    var vowel : (Char => Boolean) = (x => (x == 'a') || (x == 'e') || (x == 'i') || (x == 'o') || (x == 'u'))
    var fit1 = new FilterIterator[Char] (vowel,string1.iterator)
    print("1.1. The vowels in " + string1 + " are: ")
    while (fit1.hasNext) print(fit1.next + " ")
    println()
    // Test 1.2 - empty String
    var string2 = ""
    var gapString : (Char => Boolean) = (x => ('g' <= x) && (x <= 'p'))
    var fit2 = new FilterIterator[Char] (gapString,string2.iterator)
    print("1.2. Empty test should print nothing: ")
    while (fit2.hasNext) print(fit2.next + " ")
    println()
    // Test 1.3 - elements that verify the test at the beginning and at the end
    var string3 = "abcda"
    var letter_a : (Char => Boolean) = (x => x == 'a')
    var fit3 = new FilterIterator[Char] (letter_a,string3.iterator)
    print("1.3. There should be two a's here: ")
    while (fit3.hasNext) print(fit3.next + " ")
    println()
    /** Tests for Question 2 */
    println("-----------------------")
    println("Printing for Question 2")
    println("-----------------------")
    // Test 2.1 - test from the task
    println("---------------")
    println("Test 2.1 - task")
    println("---------------")
    val ac1 = new BasicAccount(50)
    val d10 = new DepositCommand(10)
    val w5 = new WithdrawCommand(5)
    d10.execute(ac1)
    println("Balance is: "+ac1.balance + " (60)") // Should print 60
    w5.execute(ac1)
    println("Balance is: "+ac1.balance + " (55)") // Should print 55
    // Test 2.2 - test with overdrawing the balance
    println("-------------------")
    println("Test 2.2 - overdraw")
    println("-------------------")
    val ac2 = new BasicAccount(100)
    val d100 = new DepositCommand(100)
    val w50 = new WithdrawCommand(40)
    w50.execute(ac2)
    println("Balance is: "+ac2.balance + " (60)") // Should print 60
    w50.execute(ac2)
    println("Balance is: "+ac2.balance + " (20)") // Should print 20
    w50.execute(ac2)
    println("Balance is: "+ac2.balance + " (20)") // Should print 20 (overdraw)
    // Test 2.3 - with undoing
    println("------------------")
    println("Test 2.3 - undoing")
    println("------------------")
    val ac3 = new BasicAccount(100)
    val d60 = new DepositCommand(60)
    val w30 = new WithdrawCommand(30)
    println("Balance is: "+ac3.balance + " (100)") // Should print 100
    d60.execute(ac3) match {
      case Some(ch) => ch.undo
      case _ => {}
    }
    println("Balance is: "+ac3.balance + " (100)") // Should print 100 (Deposits 60 and then uses undo)
    w30.execute(ac3)
    println("Balance is: "+ac3.balance + " (70)") // Should print 70
    var change = w30.execute(ac3)
    println("Balance is: "+ac3.balance + " (40)") // Should print 40
    change match {
      case Some(ch) => ch.undo
      case _ => {}
    }
    println("Balance is: "+ac3.balance + " (70)") // Should print 70
    // Test 2.4 - with illegal commands (negative amounts of money)
    println("-----------------------------")
    println("Test 2.4 - negative operations")
    println("------------------------------")
    val ac4 = new BasicAccount(100)
    println("Balance is: "+ac4.balance + " (100)") // Should print 100
    val invalidDeposit = new DepositCommand(-20)
    val invalidWithdraw = new WithdrawCommand(-100)
    invalidDeposit.execute(ac4)
    println("Balance is: "+ac4.balance + " (100)") // Should print 100
    invalidWithdraw.execute(ac4)
    println("Balance is: "+ac4.balance + " (100)") // Should print 100
    /** Tests for Question 4 */
    println("-----------------------")
    println("Printing for Question 4")
    println("-----------------------")
    // Test 4.1 - test from the task
    println("---------------")
    println("Test 4.1 - task")
    println("---------------")
    val ac5 = new BasicAccount(50)
    val t1 = makeTransaction(List(d10,d10,w5,d10,w5))
    val c1 = t1.execute(ac5)
    println("Balance is: "+ac5.balance + " (70)") // Should print 70
    c1.get.undo()
    println("Balance is: "+ac5.balance + " (50)") // Should print 50
    // Test 4.2 - test with invalid command
    println("--------------------------")
    println("Test 4.2 - invalid command")
    println("--------------------------")
    val ac6 = new BasicAccount(30)
    val w60 = new WithdrawCommand(60)
    val t2 = makeTransaction(List(d10,d10,w5,d10,w60,w5))
    val c2 = t2.execute(ac6)
    assert (c2 == None)
    println("Balance is:"+ac6.balance + " (30)") // Should print 30 - nothing is done
    /** Tests for Question 4 */
    println("-----------------------")
    println("Printing for Question 5")
    println("-----------------------")
    // Test 5.1 - test that ends correctly + can undo its effect
    println("-------------------------")
    println("Test 5.1 - correct + undo")
    println("-------------------------")
    val ac7 = new BasicAccount(100)
    val w10 = new WithdrawCommand(10)
    val test : (Account => Boolean) = (x => x.balance >= 30)
    val tw10 = new WhileCommand(test,w10)
    val c3 = tw10.execute(ac7)
    println("Balance is: "+ac7.balance + " (20)") // Should print 20
    c3.get.undo()
    println("Balance is: "+ac7.balance + " (100)") // Should print 100
    // Test 5.2 - test that ends incorrectly because of invalid operation
    println("--------------------")
    println("Test 5.2 - incorrect")
    println("--------------------")
    val ac8 = new BasicAccount(100)
    val tw60 = new WhileCommand(test,w60)
    assert (tw60.execute(ac8) == None)
    println("Balance is: "+ac8.balance + " (100)") // Should print 100 - nothing is done
  }
}
