data Maybe a where
    Just :: a -> (Maybe a)
    Nothing :: (Maybe a)
;

-- This is polymorphic, so can be used in match 
-- to not interfere with types.
error = error; 

xs = [1,2, 3, 4, 5];


map f ys = match ys with
    ([]) >>> [ ],
    (y:ys) >>> (f y):(map f ys);

head a = match a with
         (a:as) >>> a,
         ([]) >>> error;

main = head (map Just xs) 3;


