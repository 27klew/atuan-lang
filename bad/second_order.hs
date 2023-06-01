

data Pair a b where
	MakePair :: a -> b -> (Pair a b)
;


fun f x y = MakePair (f x) (f y);

id x = x;


main = fun id True 3;

