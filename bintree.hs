import Data.Char
data MTree a = Empty | Node a (MTree a) (MTree a) deriving (Eq, Show, Read)

singleton x = Node x Empty Empty

-- insert element into the tree
mtreeInsert x Empty = singleton x
mtreeInsert x (Node a left right) 
	| x < a = Node a (mtreeInsert x left) right 
	| x > a = Node a left (mtreeInsert x right)
	| x == a = Node x left right

-- check that element assign to tree
mtreeElem x Empty = False
mtreeElem x (Node a left right) 
	| 	x == a = True
	| 	x < a = mtreeElem x left
	| 	x > a = mtreeElem x right

-- true if binary tree is empty
mtreeEmpty x 
	| 	x == Empty = True
	| 	otherwise = False


-- print inorder
inorder Empty = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

-- print preorder
preorder Empty = []
preorder (Node x left right) = [x] ++ preorder left ++ preorder right 

-- print postorder
postorder Empty = []
postorder (Node x left right) = postorder left ++ postorder right ++ [x]

-- how many elements is in the tree
nnodes Empty = 0
nnodes (Node x left right) = length (inorder (Node x left right))

-- sum of elements
nsum Empty = 0
nsum (Node x left right) = sum (inorder (Node x left right))

-- number of leaves
leaves Empty = []
leaves (Node x left right)
	|	(left == Empty) && (right == Empty) = [x] ++ leaves left ++ leaves right
	| 	otherwise = leaves left ++ leaves right

-- tmap
tmap _ Empty = []
tmap operation (Node x left right) = tmap operation left ++ [operation x] ++ tmap operation right

-- binary tree to string
toString Empty = ""
toString (Node x left right) = "(" ++ x :[] ++ toString left ++ toString right ++ ")"

-- is balanced tree
isBalanced :: MTree a -> Bool
isBalanced (Empty) = True
isBalanced (Node x l r) = 
	let diff = abs(nnodes l - nnodes r) in 
	diff <= 1 && isBalanced l && isBalanced r

-- is binary tree
isBinary :: MTree a -> Bool
isBinary (Empty) = True;
isBinary (Node x l r) = 
	isBinary l && isBinary r
