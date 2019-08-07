-- BST implements a basic binary search tree
-- This says that a BST is either empty, or it is a Node consisting of a
--  value of type a (i.e. the data stored in the node), and two trees of
--  type BST a (i.e. the left and right subtrees of the node)
data BST a = EmptyBST | Node a (BST a) (BST a)
  deriving (Show, Read, Eq)

-- Ord a says that type a must be a type that can be compared with
--  operators like < and >=
bstInsert :: Ord a => BST a -> a -> BST a
bstInsert EmptyBST x = Node x EmptyBST EmptyBST
bstInsert (Node val left right) x = if (x < val) then
                                      Node val (bstInsert left x) right
                                    else
                                      Node val left (bstInsert right x)

-- bstContains tests if a value is in a BST or not
bstContains :: Ord a => BST a -> a -> Bool
bstContains EmptyBST _ = False
bstContains (Node val left right) x
  | val == x = True
  | val < x  = bstContains right x
  | val > x  = bstContains left x

-- An inorder traversal of a BST visits the elements in ascending
--  sorted order
bstInorder :: BST a -> [a]
bstInorder EmptyBST = []
bstInorder (Node val left right) = (bstInorder left) ++ [val] ++
                                   (bstInorder right)

-- Implementation of tree sort
-- Inserts all the elements of a list into a BST, and then extracts them
--  with bstInorder
toBST :: Ord a => [a] -> BST a
toBST [] = EmptyBST
toBST (x:xs) = bstInsert (toBST xs) x

treesort :: Ord a => [a] -> [a]
treesort = bstInorder . toBST
