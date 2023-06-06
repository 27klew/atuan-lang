
data Tree where
	NodeSimple :: Int -> Tree -> Tree -> Tree
	LeafSimple :: Tree
	;




make v = NodeSimple v LeafSimple LeafSimple;


a = NodeSimple 5 (make 3) LeafSimple;

b = match a with
	(LeafSimple) >>> -1,
	(NodeSimple x (NodeSimple y (LeafSimple) (LeafSimple)) (LeafSimple)) >>> 10 * x + y,
	(NodeSimple x r l) >>> x;
	

main = b;
