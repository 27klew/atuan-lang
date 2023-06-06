

main = let x = 2 in
	   (let f y = x + y in
	   (let x = 797 in f 1));
