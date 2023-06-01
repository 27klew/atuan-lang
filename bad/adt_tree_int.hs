-- rule: Result of GADT constructr should determine argumets' types. 
-- here: Leaf accepts a as argument, but it is not present in result (Tree Int)
-- phase: TypeCollection


data Tree a where
	Leaf :: a -> (Tree Int);


x = Leaf True;


f t = match t with 
	(Leaf a) >>> a;

-- To powinien być błąd.
y = (f x) || False;



main = x;


