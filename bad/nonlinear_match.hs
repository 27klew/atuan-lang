-- Variable can appear only once in pattern

y = [1, 2, 3];

x = match y with 
	(Cons a a) >>> 3;

main = x;

