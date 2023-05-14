

f1 x = x + 1;

f2 = (lambda x => x + 2);

fs = [f1, f2];


f = match fs with 
	(k:ks) >>> k;

main = f 0; 





