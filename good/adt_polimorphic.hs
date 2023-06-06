

data TypedTree a where 
	Node :: a -> (TypedTree a) -> (TypedTree a) -> (TypedTree a)
	Leaf :: (TypedTree a)
	;

make n = Node n Leaf Leaf;

main = Node 1 (make 2) (Node 5 (make 3) (make 4));





