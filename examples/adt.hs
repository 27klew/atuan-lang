

data Tree where
	Node :: Int -> Tree -> Tree -> Tree
	Leaf :: Tree
	;



x :: Tree = Node 4 Leaf Leaf;


y :: Int = match x with
	(Node(i, l, r)) >>> i,
	(Leaf()) >>> 0;

 
data TypedTree a where 
	Node :: a -> (Tree a) -> (Tree a) -> (Tree a)
	Leaf :: (Tree a)
	;
