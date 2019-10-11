/*
Question 1
We can replace each bucket in our hash table with a binary search tree, which will be balanced if we add another hash function
which is completely independent from the initial one. Thus, we can maintain a balanced binary search tree, so we can add data in O(log2(N))
time and also search in O(log2(N)). We should maintain the property hash2(leftchild) < hash2(parent) < hash2(rightchild) in every
binary search tree we create.
*/
