

data Tree where
	NodeSimple :: Int -> Tree -> Tree -> Tree
	LeafSimple :: Tree
	;




make v = NodeSimple v LeafSimple LeafSimple;


a = NodeSimple 5 (make 3) (make 5);

main = a;



