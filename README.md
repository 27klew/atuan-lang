

# Kompilacja 

Skrypt generate.sh generuje od nowa wszystkie pliki (i podmienia Makefile na ten spzred generacji).

Polecenie make buduje plik Atuan/Interpreter.


# Język.

Standardowy język funkcyjny, wg tabelki.

Składnia inspirowana składnią Haskella (chociaż przy pattern matchingu i lambdach są trochę inne konstrukcje).

# Typy 

Int, Bool, funkcyjne, listy.

Typy są polimorficzne, z rekonstrukcją.
Rekonstrukcja oparta jest o https://github.com/mgrabmueller/TransformersStepByStep

(TODO - brakuje przykładów do rekonstrukcji) 

Użytkownik może definiować własne typy algebraiczne,
składnia jak w rozszerzeniu GADT Haskella.
Typy argumentów konstruktora muszą się dać odtworzyć z wyniku,
tzn wszystkie zmienne typowe obecne w parametrach muszą być obecne w wyniku 

Listy są realizowane jako predefiniowany typ algebraiczny o konstruktorach Cons i Empty.


## Annotacje typów

Możliwe jest jawne podanie typów (chociaż obecnie są one ignorowane).

(TODO annotacje podane przez użytkownika nie powinny być ignorowane).

Przy kostruktorach ADT typy są konieczne.


# Lambdy

Funkcje anonimowe / domknięcia tworzone przez słowo kluczowe lambda.


# Pattern matching

Dla list dostępny jest syntax sugar z [] i : 

Poza tym mogą występować konstrukotry ADT i zmienne.
Zmienna może wystąpić w każdym wzorcu tylko raz.

Literały nie mogą występować we wzorcu.

Zagłębienie wzorców jest dowolne.

(TODO - jeśli starczy czasu można dodać sprawdzanie kompletności pattern matchingu)


# Budowa programu.

# Definicje typów

Program w tym języku składa się ciągu definicji typów i nazw globalnych.

Definicje typów mogą występować w dowolnej kolejności.
Definiują one (wzajemnie rekurencyjne, polimorficzne) typy algebraiczne.


# Definicje funkcji / stałych

Nazwy najwyższego poziomu to rekurencyjne definicje, które mogą korzystać z nazw zadeklarowanych wyżej (o ile nie zostały one przez coś przesłonięte.). 
Nazwy najwyższego poziomu są zawsze wyliczane tak jakby były funkcjami, nawet jeśli argumentów jest 0. 

Oznacza to, że jeśli nikt nie korzysta ze "stałej" najwyższego poziomu, której wyliczenie spowodowałoby np dzielenie przez zero lub nieskończąną pętlę, to błąd ten nie wystąpi.

W języku występuje let, który nie jest rekurencyjny (i jest gorliwy), oraz letrec - rekurencyjny, działa jak opisane wyżej konstrukcje z najwyższego poziomu.





# Problemy:

- składnie jest nieprzyjemna
- (TODO )ignorowane są "częściowe annotacje typów"
	tzn 

	f (x :: Bool) y = x + y

	przechodzi

	to wymaga zmiany składni tak żeby tego parser nie przyjmował. definicja powinna mieć
	podany typ lub nie.
	


