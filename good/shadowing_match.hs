
y = [1, 2, 3];

error = error;
x = match y with 
	(Cons y a) >>> y,
	([]) >>> error;

main = x;

