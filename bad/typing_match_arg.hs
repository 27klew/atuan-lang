data Tree a where
    Node :: a -> (Tree a) -> (Tree a) -> (Tree a)
    Leaf :: (Tree a)
    ;

a = Node 5 Leaf Leaf;

b a = match a with
    (Leaf) >>> True,
    (Node x r l) >>> x;
-- b może się otypować tylko do Tree Bool -> Bool
-- czyli nie powinno być aplikowane do a

main = b a;
