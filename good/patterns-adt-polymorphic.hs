

data Tree a where
	Node :: a -> (Tree a) -> (Tree a) -> (Tree a)
	Leaf :: (Tree a)
	;


make v = Node v Leaf Leaf;


a = Node 5 (make 3) (make 5);

b = match a with
	(Node x r l) >>> x,
	(Leaf) >>> 0;

main = b;