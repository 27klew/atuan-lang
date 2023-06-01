-- rule: Type names should be unique.
-- here: There are two definitions of Tree.
-- phase: TypeCollection


data Tree a where
	Node :: a -> (Tree a)
	Leaf :: (Tree a)
	;


data Tree a where
	Node2 :: a -> (Tree a)
	Leaf2 :: (Tree a)
	;
