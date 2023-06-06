-- TODO: what is happening here????
data Tree a where
    Node :: a -> (Tree a) -> (Tree a) -> (Tree a)
    Leaf :: (Tree a)
    ;


make v = Node v Leaf Leaf;


a = Leaf;

b a = match a with
    (Node x r l) >>> x,
    (Leaf) >>> 0;

-- b może się otypować do Tree Int -> Int

ll a = match a with
    (Node x r l) >>> ll l,
    (Node x Leaf l) >>> Leaf,
    (Leaf) >>> Leaf;

f x = b (ll x);

-- f : Tree Int -> Int

main = b (Node True Leaf Leaf);