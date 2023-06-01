-- rule: Only declared types can be used.
-- here: Leaf accepts non-existent Someone as argument
-- phase: TypeCollection


data Tree where
	Leaf :: Someone -> Tree
;


main = 3;