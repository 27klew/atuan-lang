


fibb x y n = 
	if n == 0 then x else  fibb (x+y) x (n-1);





main = fibb 1 1 4;

