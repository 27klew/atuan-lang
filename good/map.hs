


xs = [1,2, 3, 4, 5];


map f ys = match ys with
	([]) >>> [ ],
	(y:ys) >>> (f y):(map f ys);

f x = x + 1;


main = map f xs;

