Opis ogólny
1. Masowe uruchamianie testów hivapa na podstawie danych zebranych z internet LUB ustalonych zakresów energii i maksymalnych liczonych kanałów ewaporacji
2. Produkowanie na tej podstawie wykresów dla każdego eksperymentu na podstawie danych z hivapa przetworzonych przez hi2txt.py

TODOSY:
1. zrobienie, aby batch experiment działał - działa dobrze, gdy sigmy są duże a Z małe
   1. 10 - 278 words[w+1] index error
   2. 11 - denormalisation error
2. połączenie hipu i obróbki w jeden nadrzędny skrypt GIT
3. obliczanie kanału w nomenklaturze n,p,alpha
4. napisanie narzędzia prezentującego dane z hivap na wykresie
5. napisanie narzędzia przekształcającego dane ze scrapera w format wykorzystywany przez hi2txt aby móc je również wrzucić na wykres i ręcznie porównać
6. optymalizacja, aby tworzyło skrypt z niewiele większym maksymalnym kanałem niż jest w eksperymencie
7. pomiar czasu poszczególnego eksperymentu i zapisywanie go w pliku i na wykresie

Lista pytań:
1. O co chodzi z alphą z EvR w opisach eksperymentów, skoro czasem cząstka alpha jest uwzględniana w kanałach ewaporacji a czasem nie? Czy to są te łańcuchy alpha?
2. Jak uwzględniać różną zawartość procentową różnych izotopów w badanym materiale? Czy wystarczy wziąć różne przekroje czynne na nie z różnymi wagami?
3. Czy mogę dokończyć to po sesji? Bardzo mnie to zainteresowało, jednak na razie nie umiem przeprowadzić bardziej zaawansowanej analizy danych i muszę zdać analizę...
4. Hivap nie bada przekrojów czynnych na fuzję mniejszych niż 1nb - przy czym niektóre dane eksperymentalne (np. 58Fe + 208Pb -> 266Hs) świadczą o sigmach rzędu pb - być może coś przeoczam, a być może jest to ograniczenie oprogramowania (1994)
5. hi2txt.py nie ogarnia pierwiastków powyżej Z = 109 (Meitner, Mt) - na razie zostawiam to, aby mieć chociaż plotowanie wykresów