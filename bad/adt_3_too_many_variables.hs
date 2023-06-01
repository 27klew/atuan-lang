-- rule: Each construct should provide exactly the amount of variables, that thetype needs.
-- here: AnotherTree was declared with one variable, but Node provides three.
-- phase: TypeCollection

data AnotherTree a where
	Node :: a -> (AnotherTree a b c)
	;
