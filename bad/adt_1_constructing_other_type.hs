

data Tree where
	Node :: Int -> Tree -> Tree -> Tree
	Leaf :: Tree
	;
 
data TypedTree a where 
	TypedNode :: a -> (TypedTree a) -> (TypedTree a) -> (Tree a)
	TypedLeaf :: (Tree a)
	;
