xs = [1,2, 3, 4, 5];


map f ys = match ys with
    ([]) >>> [ ],
    (y:ys) >>> (f y):(map f ys);

f x = x + 1;

data TypedTree a where
    Node :: a -> (TypedTree a) -> (TypedTree a) -> (TypedTree a)
    Leaf :: (TypedTree a)
    ;

make n = Node n Leaf Leaf;

main = map make xs;
