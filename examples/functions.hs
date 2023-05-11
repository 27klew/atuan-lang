


f x y :: Int -> Int -> Int = 
	if x == 0 then y + 1
		else (if y == 0 then f (x-1) 1 else
			f (x-1) (f x (y-1)));




x :: Int -> Int = (lambda x y :: Int -> Int => x*y + 1);

y :: Int -> Int = x 0;


main = x;


