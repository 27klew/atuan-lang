
data Tree a where
	Node :: a -> (Tree a) -> (Tree a) -> (Tree a)
	Leaf :: (Tree a)
	;


data YetAnotherTree x y z where 
    YBNode :: a -> (YetAnotherTree a  b ((TypedTree a)))
    YANode :: a -> (YetAnotherTree a [a] b)
	;


main = 3;
