object Question4
{
  def main (args: Array[String]) =
  {
    val timeEnd:Double = 1.0
    val numSteps:Int =
    val timeStep:Double = timeEnd/numSteps
    // timeEnd=numSteps*timeStep and numSteps∈N
    var time = 0.0
    while (time < timeEnd-1e-10)
    {
      // Inv: 0 <= time <= timeEnd and time=k*timeStep for some k∈N
      time += timeStep
    }
    // Inv => time == timeEnd
  }
}
/*(a) After each iteration of the loop, time is in an interval of the form (n*timeStep-error,n*timeStep+error), where n
is the number of iterations done so far and error is a very small value than is obtained from the fact that the arithmetic we
use on floating points is not precise (somewhere close to n*epsilon) because at each step we can have an error of +- epsilon, which
is undetectable by the machine. So, after numSteps operations, time is in (timeEnd-error,timeEnd+error). If we are in the first half of
the interval, we need an additional iteration of the loop for time to exceed timeEnd, so (numSteps+1) iterations, and if not, then we
are done and we needed just numSteps iterations. (notice that the error is still extremely small compared to timeStep).
(b) At the end of the loop, if we were in the case when we needed numSteps iterations, time will be in the interval [timeEnd,timeEnd+error)
so we are very close to the value we want. However, if we needed numSteps+1 iterations, then time was in the interval (timeEnd-error,timeEnd),
so by adding a timeStep, we will be in the interval (timeEnd-error+timeStep-epsilon,timeEnd+timeStep+epsilon), so we can be at a distance
of approximately timeStep far from the result we wanted.
(c) Let's say that instead of time<timeEnd in the guard of the while we would have time<limit. Our goal is to find a limit such that
the time will be as close as possible to timeEnd at the end of the loop even in the worst case scenario. Here, we can use the fact that
after n iterations, time is in (n*timeStep-error,n*timeStep+error). By setting limit to be timeEnd-error, where error would be
epsilon*numSteps, then we know that after numSteps iterations time will be greater than the limit and in the worst case scenario, it will
be timeEnd+error, which is greater than the limit by 2*error, which is pretty efficient. I tried error to be 1e-10 and it worked very well
on numbers up to 9 digits.*/
