

a :: Int = 3;

b :: Bool = True;

c :: [Int] = [1, 2, 3];

d x :: Int -> Int = x + 1;


data SomeType where
	SomeInt :: Int -> SomeType
	SomeBool :: Bool -> SomeType
;


e :: SomeType = SomeInt 123;


main = e;

