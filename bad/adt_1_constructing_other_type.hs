-- rule: constructors should construct value of their type.
-- here: TypedNode and TypedLeaf construct Tree, but should construct TypedTree.
-- phase: Type Collection

data Tree where
	Node :: Int -> Tree -> Tree -> Tree
	Leaf :: Tree
	;
 
data TypedTree a where 
	TypedNode :: a -> (TypedTree a) -> (TypedTree a) -> Tree
	TypedLeaf :: Tree 
	;
