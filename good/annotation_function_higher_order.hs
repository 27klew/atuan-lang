

double (f :: t -> t) (x :: t) :: t = f (f x);

two x = 2*x;


main = double two 1;

