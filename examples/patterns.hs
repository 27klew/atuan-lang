x :: Int = match xs with
	([]) >>> 0,
	(x:xs) >>> 1;

y :: Int = match b with
	(True) >>> 3 + x,
	(False) >>> 4;


z :: Int = match SomeType with
  (Some(x, y)) >>> x,
  (None()) >>> 0;


w :: Int = match x with 
	(True) >>> (if 0 then x else x),
	(False) >>> 8; 



