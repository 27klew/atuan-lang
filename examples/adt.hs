

data Tree where
	NodeSimple :: Int -> Tree -> Tree -> Tree
	LeafSimple :: Tree
	;



x :: Tree = Node 4 Leaf Leaf;


y :: Int = match x with
	(Node i l r) >>> i,
	(Leaf) >>> 0;

 
data TypedTree a where 
	Node :: a -> (TypedTree a) -> (TypedTree a) -> (TypedTree a)
	Leaf :: (TypedTree a)
	;

x :: (TypedTree ((TypedTree Int))) = 3;



data AnotherTree x y z where
	ANode :: a -> (AnotherTree a b c)
	;



data YetAnotherTree x y z where 
    YBNode :: a -> (YetAnotherTree a  b ((TypedTree a)))
    YANode :: a -> (YetAnotherTree a [a] b)
	;


main = Node 3 Leaf Leaf;

