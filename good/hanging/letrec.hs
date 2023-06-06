-- This shoudl type correctly to int,
-- but should hang during exetution.
y = letrec x = x + 1 in x;
main = y;
