--  Litterals in pattern matching are not (currently) allowed.


xs = [1, 2, 3];


main = match xs with 
	(1:k) >>> k;

