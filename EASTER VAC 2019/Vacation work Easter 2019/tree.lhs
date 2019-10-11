> data Tree = Leaf | Fork Tree Tree

> preorder :: Tree -> [Bool]
> preorder Leaf = [False]
> preorder (Fork l r) = True : (preorder l) ++ (preorder r)
