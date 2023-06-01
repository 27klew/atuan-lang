-- rule: Constructor names should be unique.
-- here: Tree and AnotherTree both have constructor Node. 
-- phase: TypeCollection

data Tree where
	Node :: Int -> Tree -> Tree -> Tree
	Leaf :: Tree
	;


data AnotherTree x y z where
	Node :: a -> (AnotherTree a b c)
	;


main = 3;