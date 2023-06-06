

f1 x = x + 1;

f2 = (lambda x => x + 2);

fs2 = [f1, f2];


f fs = match fs with 
	(k:ks) >>> k,
	([]) >>> (lambda x => x)
	;

main = f fs2 0; 





