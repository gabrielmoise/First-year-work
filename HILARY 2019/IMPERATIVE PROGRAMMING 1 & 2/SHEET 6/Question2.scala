// Question 2
/*
Each item has a probability of 1/N to go in any specific bucket because their hash functions are uniform on [0..N).
Therefore, the distribution of the number of items that go into a specific bucket is Bin(n,1/N), and the mean of this is n/N.
For an unsuccessful search, we compare the item with every item from the bucket, so the distribution of the number of comparisons
for an unsuccessful search is the same, with the expectated number of comparisons n/N.
For a successful search, let's suppose that we have k keys to retrieve and the number of operations needed to retreive all of them is
1 + 2 + ... + k = k(k+1)/2. So, if the number of items is X, which is a random variable, the expectation for this is:
E = sum for k (k(k+1)/2 * P(X = k)). Since X has binomial distribution Bin(n,1/N), we get:
E = 1/2*(E(X^2) + E(X)) = n/N * ((n-1)/2*N - 1)
So, if we want to get the mean cost per key retrieved we divide by n and to obtain the total expected cost we multiply by N to get :
E = (n-1)/2*N - 1
*/
