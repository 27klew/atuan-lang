

data Tree a where
	Node :: a -> (Tree a) -> (Tree a) -> (Tree a)
	Leaf :: (Tree a)
	;


a = Node 5 Leaf Leaf;

b = match a with
	(Leaf) >>> True,
	(Node x r l) >>> x;

main = b;