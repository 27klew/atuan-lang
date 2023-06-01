-- Mismatch of types in pattern match branches.

x = [1, 2, 3];
y = match x with  
   ([]) >>> True, -- Bool
   (k:ks) >>> k;  -- Int
main = y;


