xs = [ ] ; 
y = match xs with 
	([]) >>> 1,
	(k:ks) >>> k;
main = y;




xs2 = 2 : [ ]  ; 

y2 = match xs2 with 
	([]) >>> -3,
	(k:ks) >>> k;

main = y + 10 * y2;


