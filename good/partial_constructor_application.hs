

data Maybe a where 
	Just :: a -> (Maybe a)
	Nothing :: (Maybe a)
;


xs = [1,2, 3, 4, 5];


map f ys = match ys with
	([]) >>> [ ],
	(y:ys) >>> (f y):(map f ys);


main = map Just xs;

