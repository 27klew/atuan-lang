


-- This lambda captures environment it was created in.
make x y = (lambda z => x * z + y);

f = make 10 4;

f2 = make 1 1;

main = f 7 + f2 1;

