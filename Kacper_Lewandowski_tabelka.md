  Na 20 punktów
  01  + (dwa typy)
  02  + (arytmetyka, porównania)
  03  + (if)
  04  + (funkcje wieloargumentowe, rekurencja)
  05  + (funkcje anonimowe i wyższego rzędu, częściowa aplikacja)
  06  [?+] (obsługa błędów wykonania)
  07  + (statyczne wiązanie identyfikatorów)
  Listy:
  08  + (z pattern matchingiem)
  09  - (z empty, head, tail)
  10  + (lukier)
  Na 25 punktów
  11 + (listy dowolnego typu, zagnieżdżone i listy funkcji)
  12 [?-] (proste typy algebraiczne z jednopoziomowym pattern matchingiem)
  13 + (statyczne typowanie)
  Na 30 punktów
  14 [?-] (ogólne polimorficzne i rekurencyjne typy algebraiczne)
  15 [?-] (zagnieżdżony pattern matching)
  Bonus
  16 + (typy polimorficzne z algorytmem rekonstrukcji typów)
  17 - (sprawdzenie kompletności pattern matchingu)


Uwagi:
6. W przypadku błędu użytkownik otrzyma informację jaki błąd wystąpił.
   Nie otrzyma informacji gdzie błąd wystąpił (to zamierzam uzupełnić)

12. Proste typy algebraiczne są, w szzcególności użytkonik może deklarować i konstruować takie typy.
    Listy realizowane są jako ADT. Pattern matching działa na razie tylko dla list. 
    (mam większość maszynerii potrzebnej by działał też dla reszty).

13. Statyczne typowanie realizowane jest przez inferencję typów (typy są polimorficzne, pierwszego rzędu).
    Uzytkownik może dopisywać w wielu miejscach informacje o typach, w tym momencie są one traktowane jako komentarze.
    (W przyszłości można sprawić, by wymuszały one konkretną postać typu.)

14. Typy dają się deklarować, ich wartości konstruować, podlegają rekonstrukcji typów, ale pattern matching działa tylko dla list
    (A więc można z wartości skorzystać tylko przypisując ją do wartości specjalnej main, która jest wypisywana dla użytkownika na ekran)

15. Obecnie pattern matching działa zagnieżdzony (ale działa tylko dla list).


