


f x y =
	x + (let x = 2*y in (let y = 3*x in y));


main = f 10 2;

