main = 
	let x :: Int = 15 in 
		(let f (y :: Int) :: Int = x * y 
			in (let x :: Bool = True in f 2));
