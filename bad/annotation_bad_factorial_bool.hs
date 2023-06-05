

fact (x :: Bool) :: Int = if x <= 0 then 1 else x * fact(x-1);


main = fact 3;


