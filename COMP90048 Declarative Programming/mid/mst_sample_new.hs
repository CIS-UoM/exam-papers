-- Question 1:  what are the types of these:
-- "abc" ++ "def"
--    String or [Char]
-- (+)
--    Num a => a -> a -> a
-- map length
--    [[a]] -> [Int] or Foldable t => [t a] -> [Int]
-- length : "xyz"
--    Type error
-- [1, 2, 3]
--    Num a => [a]



-- Question 2:  What does this do:
q2 _ _ [] = []
q2 a b (c:cs) =
    if a == c then b:(q2 a b cs)
    else c:(q2 a b cs)

-- q2 a b list replaces a's by b's in list


-- Question 3:  What does this do:
q3 a (b:[]) = a b
q3 a (b1:b2:bs) = q3 a (b2:bs)

-- q3 a list returns a applied to the last element of list

-- Question 4

type HTML = [HTML_element]
data HTML_element
    = HTML_text String
    | HTML_font Font_tag HTML
    | HTML_p HTML
    | HTML_ul [HTML]
    | HTML_ol [HTML]
    deriving (Show)
data Font_tag = Font_tag String
    deriving (Show)


-- removes all font tags (replacing them with their HTML contents).
strip_font_tags :: HTML -> HTML
strip_font_tags [] = []
strip_font_tags (HTML_text str:rest) =
  HTML_text str:strip_font_tags rest
strip_font_tags (HTML_font _ body:rest) =
  strip_font_tags body ++ strip_font_tags rest
strip_font_tags (HTML_p body:rest) =
  HTML_p (strip_font_tags body):strip_font_tags rest
strip_font_tags (HTML_ul bodies:rest) =
  HTML_ul (map strip_font_tags bodies):strip_font_tags rest
strip_font_tags (HTML_ol bodies:rest) =
  HTML_ol (map strip_font_tags bodies):strip_font_tags rest



-- Question 5

-- Multi-way trees,
-- trees in which a node may have an arbitrary number of children,
-- can be represented by a type like this,
-- which puts all the children of a node into a list:

data Mtree a = Mnode a [Mtree a]

-- Returns a multi-line string depicting an Mtree.
-- Each node should be placed on a separate line.
-- You can include \n in the string for a newline.
-- The children of a node should be placed on subsequent lines,
-- indented by one more space than the line giving the value in the node.
--
-- For example, given the tree
--     Mnode 1 [Mnode 2 [], Mnode 3 [Mnode 4 []]]
-- the output string should be
-- 
-- 1
--  2
--  3
--   4
showMtree :: Show a => Mtree a -> IO()
showMtree tree = showMtree' "" tree

showMtree' :: Show a => String -> Mtree a -> IO()
showMtree' indent (Mnode label subtrees) =
  indent ++ show label ++ "\n" ++ concatMap (showMtree' (' ':indent)) subtrees
