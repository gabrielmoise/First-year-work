object Question3
{
  // 1 <= X <= 1000
  val X = 542
  def tooBig (y : BigInt) : Boolean =
    {
      y>X
    }
  def find (l : BigInt) : BigInt =
    {
      // We will call find(1) to find X at part (b)
      var left = l
      // Invariant I : left/2 <= X
      while (tooBig(left) == false) left = left * 2
      // I holds => left/2 <= X
      // tooBig(left) == true => left > X
      // From these 2, we set (left,right) = (left/2,left)
      var right = left
      left = left / 2
      search(left,right)
    }
  def search (l : BigInt, r : BigInt) : BigInt =
    {
      // We will call search(1,1001) to find X at part (a)
      var left = l ; var right = r
      // left <= X < right
      while (left + 1  < right)
      {
        var mid = (left+right)/2
        // left < mid < right
        if (tooBig(mid)) right = mid
           else left = mid
        // I
      }
      // left + 1 = right => X = left
      left
    }
}
/**
(a)
Let D = right-left. We terminate the loop only when D = 1.
Initially, D = 1000. After each iteration, if D is even, D becomes D/2, and if it's odd, it can become D/2 or D/2+1. So, the sequence of
possible D's is :
1000 -> 500 -> 250 -> 125 -> 62/63 -> 31/32 -> 15/16 -> 7/8 -> 3/4 -> 1/2. Here, we made 9 calls to "tooBig". In the case that D is 1 after
this, we stop, otherwise we make another call to get D = 1. So in total, we can have either 9 or 10 calls to "tooBig".
Because we halving D at each step, we can deduct that the search function needs log2(D) or log2(D)+1 operations.
(b)
1. We start by finding lower and upper bounds for X : from k = 0, we search for k such that 2^k <= X < 2^(k+1), so we will get
tooBig(2^k) = false and tooBig(2^(k+1)) = true -> this is done in O(log2(X) as k=log2(X) and we perform (k+1) calls of "tooBig").
2. We apply search(2^k,2^(k+1)) which needs at most log2(D)+1, as we have seen at part (a), where here D=2^(k+1)-2^k=2^k, so we again need
k operations, so O(log2(X)).
In the end we have time complexity of O(log2(X)).
(c)
Let t = 2^(1/epsilon)
1. We first find an upper bound for X, by finding k such that t^k <= X < t^(k+1). This needs log in base t of X steps.
2. We can find X with a binary search from t^k to t^(k+1), which needs log2(t^(k+1)-t^k) = log2(t^k) + log2(t-1) =
log2(X) + log2(2^(1/epsilon)-1) steps(rounded upwards for non-integer numbers).
So, the number of steps we need in total is :
logt(X) + log2(X) + log2(2^(1/epsilon)-1) = epsilon*log2(X) + log2(X) + 1/epsilon
We approximated log2(2^(1/epsilon)-1) with 1/epsilon as we only treat here the cases where 0<epsilon<1 (for epsilon > 1 we already
have a method that finds X in 2log2(X) steps which is less than (1+epsilon)log2(X)). Therefore, 1/epsilon is bigger than 1 so the -1
from the logarithm is negligible.
Therefore, we obtained a total of (1+epsilon)log2(X) + r(epsilon), where r(epsilon) = 1/epsilon
*/
