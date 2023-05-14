


-- Tutaj są dwie wartości całkowite
x = 4;
y = 5;


-- To ma typ Bool
cond = x * y < x + y;



w = if cond then -x else x;

z = if cond && (x > 0) then y else (2*y);


main = w + z;

