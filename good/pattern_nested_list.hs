

xs1 = [1, 2];
xs2 = [3, 4];

xs = [xs1, xs2];


-- ys = match xs with 
-- 	((f:fs):ks) >>> f;

ys = match xs with 
	((f:fs):aa) >>> f;


main = ys;

