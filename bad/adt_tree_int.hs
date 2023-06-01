data Tree a where
	Leaf :: a -> (Tree Int);


x = Leaf True;


f t = match t with 
	(Leaf a) >>> a;

-- To powinien być błąd.
y = (f x) || False;



main = x;


