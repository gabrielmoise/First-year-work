/**
Question 5
If the array is sorted decreasingly, the partition function will need linear time and will partition each array
into a singleton(the first element) and the N-1 elements remaining. So, the complexity of quickSort will be :
T(N) = T(N-1) + O(N), which mean that T(N) = O(N^2).
By splitting the array randomly, the large array will be on average of size 3*N/4, as the larger sub-array is always
of length between N/2 and N, so by uniformity, on average we get 3*N/4.
Thus, the complexity of the quicksort will be:
T(N) = T(N/4) + T(3*N/4) + O(N), which runs in O(N*log(N)) and it can be solved with a recurrence tree.
*/
