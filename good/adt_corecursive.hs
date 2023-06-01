

data A a where
	AA :: a -> (A a)
	AB :: (B a) -> (A a)
;

data B a where
	BB :: a -> (B a)
	BA :: (A a) -> (B a)
;



bb = BB 3;

ab = AB bb;


main = ab;
