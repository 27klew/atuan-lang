
Standardowy język funkcyjny, wg tabelki. Docelowo będzie na ok 30 pkt.

Składnia inspirowana składnią Haskella (chociaż przy pattern matchingu i lambdach są trochę inne konstrukcje).

Typy: Int, Bool, funkcyjne, listy. 
Tworzenie typów algebraicznych jak w w rozszerzeniu GADTs Haskella (jawne podanie typów konstruktorów - przykład w adt.hs).
W szczególności są one polimorficzne.

Funkcje anonimowe / domknięcia tworzone przez słowo kluczowe lambda.

Pattern matching: dla list po [] | x:xs, po kontruktorach ADT. Dowolne zagłebienie wzorców.


Możliwe jest jawne podanie typów (chociaż obecnie są one ignorowane).
Zaimplementowana jest rekonstrukcja typów, oparta o podany na Moodle (https://github.com/mgrabmueller/TransformersStepByStep).
(To jest na razie raczej prototyp, przedyskutuję go przy prezentowaniu / na najbliższych zajęciach).




Program w tym języku składa się ciągu definicji typów i nazw globalnych.

Definicje typów mogą występować w dowolnej kolejności.
Mogą one deklarować (wzajemnie rekurencyjne) typy algebraiczne, poprzez wypisanie ich konstrukotorów, wraz z jawnym podaniem typów.
(Jest to jedyne miejsce w języku gdzie podanie wszystkiech typów jest konieczne).

Nazwy globalne to rekurencyjne definicje, które mogą korzystać z nazw zadeklarowanych wyżej (o ile nie zostały one przez coś przesłonięte.). Nazwy globalne są zawsze wyliczane  tak jakby były funkcjami, nawet jeśli argumentów jest 0. Sprawia to m.in., że program, który nie używa globalnej wartości, której obliczenie powodowałoby błąd, może wykonać się poprawnie (o ile nie wystąpią inne błędy).

Let nie jest rekurencyjny. (Letrec jest).


Interpreter kompilować należy poleceniem `ghc Atuan/Interpreter.hs`
(Względem outputu bnfc jest konieczna jedna drobna zmiana w pliku Abs.hs)


GADT są ograniczone: w wyniku muszą pojawić się wszystkie zmienne, które występują w parametrach konstruktora.


Pattern Matching nie wspiera literałów.





