

data Tree where
	NodeSimple :: Int -> Tree -> Tree -> Tree
	LeafSimple :: Tree
	;




make v = NodeSimple v LeafSimple LeafSimple;


a = NodeSimple 5 (make 3) (make 5);

b = match a with
	(NodeSimple x r l) >>> x,
	(LeafSimple) >>> 0;

main = b;

