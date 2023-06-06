-- This will be rejected by parser
-- Partial annotation are not allowed.Doc

-- Do [annotate] or not. There is no try.
f (x :: Bool) y = x + y;

main = f;