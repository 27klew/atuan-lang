-- rule: variables should be lowercase
-- here: variable A of AnotherTree starts with uppercase A.
-- phase: Type Collection

data AnotherTree A where
	Node :: a -> (AnotherTree a)
	;
