
Standardowy język funkcyjny, wg tabelki, na 25 pkt (na 30 pkt jeśli starczy czasu).

Składnia inspirowana składnią Haskella.

Typy: Int, Bool, funkcyjne, listy. 
Tworzenie typów algebraicznych jak w w rozszerzeniu GADTs Haskella (jawne podanie typów konstruktorów - przykład w adt.hs).

Funkcje anonimowe / domnięcia tworzone przez słowo kluczowe lambda.

Pattern matching: dla list po [] | x:xs, po kontruktorach ADT, po literałach. 
W pierszej wersji jednopoziomowy, domyślnie będzie wielopoziomowy.


Możliwe jest jawne podanie typów (chociaż obecnie są one ignorowane).
Zaimplementowana jest rekonstrukcja typów, oparta o podany na Moodle (https://github.com/mgrabmueller/TransformersStepByStep).
(To jest na razie raczej prototyp, przedstawię go na zajęciach).


ADT są polimorficzne.



Program w tym języku składa się ciągu definicji typów i nazw globalnych.

Definicje typów mogą występować w dowolnej kolejności.
Mogą one deklarować (wzajemnie rekurencyjne) typy algebraiczne, poprzez wypisanie ich konstrukotorów, wraz z jawnym podaniem typów.
(Jest to jedyne miejsce w języku gdzie podanie wszystkiech typów jest konieczne).

Nazwy globalne to rekurencyjne definicje, które mogą korzystać z nazw zadeklarowanych wyżej (o ile nie zostały one przez coś przesłonięte.). Nazwy globalne są zawsze wyliczane  tak jakby były funkcjami, nawet jeśli argumentów jest 0. Sprawia to m.in., że program, który nie używa globalnej wartości, której obliczenie powodowałoby błąd, może wykonać się poprawnie (o ile nie wystąpią inne błędy).


Język wspiera pattern-matching (tylko po listach).












