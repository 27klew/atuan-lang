-- rule: Each construct should provide exactly the amount of variables, that thetype needs.
-- here: AnotherTree was declared with three variables, but Node provides only one.
-- phase: TypeCollection

data AnotherTree a b c where
	Node :: a -> (AnotherTree a)
	;
