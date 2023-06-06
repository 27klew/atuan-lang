

id (x :: Int) :: Int = x + 1;


data A3 a where 
	Make :: (A3 Int);

y = Make;

-- Tutaj A3 jest typem (obecnie nie jest sprawdzane czy typy które są
-- 	w annotacjach istnieją, ale jeśli użytkownik taki poda to i tak powstanie błąd typów
-- )
id2 (x :: A3) :: A3 = x;


id3 (x :: A3) :: A3 = y;


main = id2 y;
