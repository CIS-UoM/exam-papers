-- Question 1 (2 marks)

-- For each of the following Haskell expressions,
-- write down its \textbf{type} (which may be a function type
-- or may include type class constraints)
-- or say that it represents a type error.

--   []
--         [a]
--   (++)
--         [a] -> [a] -> [a]
--   map
--         (a->b) -> [a] -> [b]
--   show
--         Show a => a -> String
--   (. length)
--         (Int -> a) -> [b] -> a


-- Question 2 (2 marks)

-- Briefly explain what this Haskell function does.
-- Give a type declaration for q2.

q2 _ [] = 0
q2 a (b:bs) = c + q2 a bs
    where c = if a == b then 1 else 0

-- q2 :: (Eq a, Num b) => a -> [a] -> b
-- (but I'll accept q2 :: Eq a => a -> [a] -> Int)
--
-- q2 a lst returns the number of occurrences of a in lst



-- Question 3 (3 marks)

-- Write a function 'merge' that takes two sorted lists as input
-- and returns a sorted list of all the elements of both lists by
-- interleaving them as necessary to put them in order.
-- Include a type declaration for your function.

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge as [] = as
merge [] bs = bs
merge (a:as) (b:bs)
  | a <= b    = a:merge as (b:bs)
  | otherwise = b:merge (a:as) bs



-- Question 4 (3 marks)

-- Given the following binary search tree (BST) data type

data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving Show  -- I added this for testing

-- write a Haskell function \texttt{insert} to insert an element into a tree.
-- Recall that in a BST, every element appearing in the first (left) subtree
-- of a node must be smaller than the element of that node, and every element
-- in the second (right) subtree must be larger.
-- Note that this tree type does not have values, just keys. Include a type
-- declaration for your \texttt{insert} function.
-- You may use any function in the Haskell prelude, but not functions
-- defined in libraries.

insert :: Ord a => a -> Tree a -> Tree a
insert a Empty = Node Empty a Empty
insert a (Node left b right)
  | a == b    = Node left b right
  | a <  b    = Node (insert a left) b right
  | otherwise = Node left b (insert a right)
