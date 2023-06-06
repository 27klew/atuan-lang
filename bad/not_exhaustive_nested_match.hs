

data Tree where 
	Leaf :: Tree
	Node :: Int -> Tree -> Tree -> Tree;



get a = match a with 
	([]) >>> 3,
	((Node x y z):xs) >>> x;

main = 3;

