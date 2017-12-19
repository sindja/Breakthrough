;----------------------------------------------------------------------------------------------------------------------------------------------------------------
;                                               Radi razbijanja monotonije naseg Al igraca nazvali smo Djema :D
;----------------------------------------------------------------------------------------------------------------------------------------------------------------
;                                                                     Zadatak 1 
;----------------------------------------------------------------------------------------------------------------------------------------------------------------
;Koristićemo matricu predstavljenu preko lancanih listi za predstavljanje table na kojoj se odvija igra. Svako polje na tabli je predstavljeno sa lancanom listom u kojoj je prvi element vrsta, drugi kolona i treci vrednost tog polja na tabli (koja figura se nalazi na njemu, ili je prazno).
;----------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funkcija start() se poziva na početku igre, i u sebi sadrži pozive funkcija koje se koriste za postavljanje početnog stanja. U njoj se vrši kreiranje i postavljanje globalne promenljive tabla, koja predstavlja tablu za igru, kao i izbor redosleda igranja.
(defun start()
	(kreiraj_globalne_promenljive)
	(unos_dimenzija)
	(defvar tabla)
	(setq tabla (kreiraj_tablu m n))
	(izbor_redosleda)
  (stampaj tabla)
	)


;Funkcija kreiraj_globalne_promenljive() vrši kreiranje globalnih promenljiva i postavljanje početnih vrednosti istima. Promenljiva m je broj vrsta, n broj kolona na tabli za igru. Promenljiva kraj se koristi pri testiranju ciljnog stanja tj. da li se došlo do kraja igre. Promenljiva nasao se koristi pri testiranju postojanja figura na tabli. prvi_igrac je za čuvanje prvog igrača, a player na trenutnog igrača.
(defun kreiraj_globalne_promenljive()
	(defvar m)
	(defvar n)
  	(defvar kraj)	
    	(setq kraj '0)
  	(defvar  nasao)
    	(setq nasao '0)
    	(defvar prvi_igrac)
    	(defvar player)
    	(setq player '0)
	)	


;Funkcija unos_dimenzija() se koristi za unošenje željene veličine table od strane igrača. Sa tastature se učitavaju vrednosti za broj vrsta i broj kolona respektivno, uz štampanje određenih poruka pre samog unosa. Za štampanje je iskorišćena funkcija format.
(defun unos_dimenzija()
	(format t "Unesite broj vrsta.~%")
		(setq m (read))
		(format t "Unesite broj kolona, maksimalno 26 zbog ogranicenosti brojem slova. ~%")
		(setq n (read))
		)

;Funkcija kreiraj_tablu(v k) vrši kreiranje table sa v vrsta i k kolona kao i postavljanje početnog stanja na tabli za igru. U sebi sadrži rekurzivni poziv za broj vrsta umanjen za 1 i isti broj kolona sve dok broj vrsta ne dodje do 0, kao i poziv dodatne funkcije dodaj_kolone.
(defun kreiraj_tablu(vrs kol)
	(cond ((= vrs '0) '())
		(t (append (kreiraj_tablu (- vrs 1) kol) (list (dodaj_kolone vrs kol))))
		))

;Funkcija dodaj_kolone(v k) formira k kolona za v-tu vrstu. Rekurzivno se poziva sa brojem kolona umanjen za 1, sve dok broj kolona ne dodje do 0. Poziva dodatnu funkciju dodaj_element.
(defun dodaj_kolone(vrsta kolona)
	(cond ((= kolona 0) '())
		(t (append  (append (dodaj_kolone vrsta (- kolona 1)) '()) (list (dodaj_element vrsta kolona)) ))
		))

;Funkcija dodaj_element(v k) formira jedan element matrice tj. polje na tabli, kao listu	koja sadrži broj vrste, broj kolone i vrednost samog polja na tabli.
(defun dodaj_element(v k)
	(list  v k (cond ((equalp v '1) '1)
		((equalp v '2) '1)
		((equalp v (- m 1)) '2)
		((equalp m v) '2)
		(t '0)
		)
	))
	
;Izgled formirane table sa početnim stanjem predstavljene matricom za po 8 vrsta i kolona:
;(((1 1 1) (1 2 1) (1 3 1) (1 4 1) (1 5 1) (1 6 1) (1 7 1) (1 8 1))
 ;((2 1 1) (2 2 1) (2 3 1) (2 4 1) (2 5 1) (2 6 1) (2 7 1) (2 8 1))
 ;((3 1 0) (3 2 0) (3 3 0) (3 4 0) (3 5 0) (3 6 0) (3 7 0) (3 8 0))
 ;((4 1 0) (4 2 0) (4 3 0) (4 4 0) (4 5 0) (4 6 0) (4 7 0) (4 8 0))
; ((5 1 0) (5 2 0) (5 3 0) (5 4 0) (5 5 0) (5 6 0) (5 7 0) (5 8 0))
; ((6 1 0) (6 2 0) (6 3 0) (6 4 0) (6 5 0) (6 6 0) (6 7 0) (6 8 0))
; ((7 1 2) (7 2 2) (7 3 2) (7 4 2) (7 5 2) (7 6 2) (7 7 2) (7 8 2))
 ;((8 1 2) (8 2 2) (8 3 2) (8 4 2) (8 5 2) (8 6 2) (8 7 2) (8 8 2)))


;Funkcija stampaj(l) prikazuje trenutno stanje igre. Parametar ove funkcije je tabla na kojoj se igra. Sadrži poziv dodatne funkcije stampaj_kolone, stampaj_slova kao i rekurzivni poziv za ostatak table (liste).
(defun stampaj(lista)
	(cond((null lista) (stampaj_slova n))
		(t (stampaj_vrstu (car lista)) (stampaj (cdr lista)) )
		))

;Funkcija stampaj_vrstu (vrsta) prikazuje na ekranu jednu vrstu iz matrice, uz prikaz broja vrste ispred svake od njih.	Poziva se rekurzivno dok postoji elemenata u vrsti uz poziv dodatne funkcije stampaj_element za svaki element vrste.
(defun stampaj_vrstu (lista)
	(cond ((null lista) '())
		(t (princ (caar lista)) (if (> (caar lista) 9) (format t " ") (format t "  ")) (stampaj_element lista))
		))

;Funkcija stampaj_element (element) prikazuje figuru na određenom polju na tabli ako postoji neka figura na njemu ili ostavlja prazno ako nema figure na njemu.
(defun stampaj_element (lista)
	(cond ((null lista) (format t "~%"))
        (t (cond ((null lista) (format t "~%"))
            ((equal (car (reverse (car lista))) 0) (format t " "))
			((equal (car (reverse (car lista))) 1) (format t "X"))
            ((equal (car (reverse (car lista))) 2) (format t "O"))
            (t (format t " "))) (format t " ") (stampaj_element (cdr lista))))
       )


;Funkcije stampaj_slova(n) i slova(n) formiraju poslednju vrstu za prikaz koja se sastoji od n slova tj. od onoliko slova koliki je broj kolona na tabli, po abecednom redosledu. Koristi se funkcija code-char za prevodjenje broja u karakter.
(defun stampaj_slova (n)
	(format t " ") (format t "  ") (slova n)
		)
 		
(defun slova(k)
	(cond ((equalp k '0) (format t "~%"))
		(t (princ (code-char (+ 65 (- n k)))) (format t " ") (slova (- k 1)))
		))		

;Izgled  prikazanog  trenutnog stanja na ekranu na početku igre:

;1 X X X X X X X X 
;2 X X X X X X X X 
;3                 
;4                 
;5                 
;6                 
;7 O O O O O O O O 
;8 O O O O O O O O 
;  A B C D E F G H 

;Funckija za izbor redosleda igranja prilikom startovanja igre. Igrac bira izmedju igranja sa X, tj. unos jedinice, i igranja sa O, tj. unos 0. X uvek igra prvi.
(defun izbor_redosleda()
    (format t "unesite 0 ako zelite da prvi igra racunar ili unesite 1 ako vi zelite da igrate prvi.~%")
     (setq prvi_igrac (read))
      (cond ((equal prvi_igrac 0) (igraj 0)) ((equalp prvi_igrac 1) (igraj 1)) (t (format t "pogresno ste uneli. ~%")(izbor_redosleda) ))
	)

;Funkcija koja obezbedjuje naizmenicno povlacenje poteza, od strane racunara i igraca i proverava da li imamo pobednika. Binarni parametar el odredjuje, koji je igrac trenutno na potezu
(defun igraj (el)
     (cond ((equalp el '0) (format t "igra X .~%")(djema_igra))
      (t(format t "igra O ~%") (covek_igra)))
      (proveri_kraj)
      (if (equalp kraj '0) (igraj (mod (+ el 1) 2)) )
      ) 

;Funkcija koja obezbedjuje coveku unos poteza koji zeli da odigra
(defun covek_igra()
       (stampaj tabla)
       (unesi_potez)
        )

;Funkcija u kojoj racunar(Djema) povlaci svoj potez
(defun djema_igra()
               (stampaj tabla)
     (format t "ovde igra Djema.~%")
               (setq tabla (car (minimax tabla 2 -500 500 player)))
       
     (setq player (mod (+ player 1) 2))
      )


;Funkcija za unos poteza obezbedjuje unos pozicije figure koju zelite da pomerite, prikazuje moguce poteze i obezbedjuje unos pozicije na koju zelite da pomerite navedenu figuru. U promenljivoj stara_pozicija se cuva pozicija figure koju zelite da pomerite, promenljiva potezi predstavlja listu mogucih poteza, dok je promenljiva nova_pozicija zeljena nova pozicija
(defun unesi_potez ()
 	(setq stara_pozicija (unesite_staru_poziciju))
  
 	(setq potezi (moguci_potezi stara_pozicija))
        (cond ((null potezi) (unesi_potez)) 

 		(t (format t "Moguci potezi.~%")
 		(format t "~a~%" potezi)
 		(setq nova_pozicija (unesite_novu_poziciju potezi))
		(povuci_potez stara_pozicija nova_pozicija)
 		)))



;Funkcija unesite_staru_poziciju() se koristi za unos pozicije sa koje igrač želi da pomeri figuru. Ova funkcija vraća unetu poziciju ako ona ispunjava uslove, koji se proveravaju dodatnom funkcijom proveri_staru_poziciju(spozicija). Ukoliko je uneta pogrešna pozicija obaveštava se igrač i zahteva se ponovno unošenje.
(defun unesite_staru_poziciju()
 	(format t "Unesite poziciju figure koju zelite da pomerite. ~%")
 		(setq stara (reverse(list (konvertuj_slovo-broj(read)) (read))))
 		(cond ((proveri_staru_poziciju stara) stara)
 		(t (format t "Greska.~%") (unesite_staru_poziciju))
 		))
 

;Funkcija proveri_staru_poziciju(spozicija) proverava poziciju koju je uneo trenutni igrač a sa koje želi da pomeri figuru. Vraća vrednost true ukoliko na njoj postoji figura trenutnog igrača ili false ukoliko ne postoji figura trenutnog igrača.
(defun proveri_staru_poziciju(spozicija)
 	(if (equalp player '0) (if ( equalp (car (reverse (nth (- (cadr spozicija) 1) (nth (- (car spozicija) 1) tabla)))) '1) t '()) ; than
 		(if ( equalp (car (reverse (nth (- (cadr spozicija) 1) (nth (- (car spozicija) 1) tabla)))) '2) t '()); else
 		))



;Funkcija za formiranje i vracanje liste mogucih poteza, prolaskom i proverom susednih polja navedenog polja.
(defun moguci_potezi (stara)
 	(cond ((equalp player '0) (remove '() (list (proveri_element (+ (car stara) '1 ) (-(cadr stara) '1) '0 '0) (proveri_element (+ (car stara) '1) (cadr stara)  '0 '1)  
 		(proveri_element (+ (car stara) '1) (+(cadr stara) '1) '0 '0))))
 	((equalp player '1) (remove '() (list  (proveri_element (- (car stara) '1 ) (-(cadr stara) '1) '1 '0) (proveri_element (- (car stara) '1) (cadr stara)  '1 '1)  
 		(proveri_element (- (car stara) '1) (+(cadr stara) '1) '1 '0))  ))
 	))


;Provera da li je moguce kretanje figure na navedenu poziciju. Parametri el1 i el2 su koordinate na tabli, el3 je indikator igraca koji je na potezu, dok je pravo-koso indikator dozvole igracu da pojede protivnicku figuru, jer figure mogu da jedu samo koso.
(defun proveri_element  (el1 el2 el3 pravo-koso )
    (if (equalp pravo-koso '1)
     (if (equalp (car(reverse(nth (- el2 1) (nth (- el1 1) tabla)))) '0) (reverse(list  el1 (konvertuj_broj-slovo el2))));than
      (if (equalp pravo-koso '0) (cond ((equalp el2 0) '())           ;else
            ((equalp el2 (+ n 1))  '())
            (t (if (equalp (+ el3 1) (car(reverse(nth (- el2 1) (nth (- el1 1) tabla))))) '() (reverse(list  el1 (konvertuj_broj-slovo el2)) )))))))

;Funkcija koja obezbedjuje unos pozicije na koju zelite da pomerite figuru. Parametar je lista mogucih poteza. Funkcija se poziva rekurzivno ukoliko je igrac pogresno uneo novu poziciju.
(defun unesite_novu_poziciju (lista_potega)
 	(format t "Unesite novu poziciju.~%")
 	(setq nova_pozicija  (list (read) (read)))
 	(cond ((member_potezi nova_pozicija lista_potega) (list (cadr nova_pozicija) (konvertuj_slovo-broj (car nova_pozicija))))
 	 (t (format t "Greska.~%") (unesite_novu_poziciju lista_potega)))
 	)

;Funkcija koja proverava da li je nova pozicija figure validna, tj. da li se nalazi u listi mogucih poteza. Ukoliko je potez validan vraca true, u suprotnom false.
(defun member_potezi (nova_pozicija lista_potega)
 	(cond((null lista_potega) '())
 		(t (if(proveri_poteg (car lista_potega) nova_pozicija) t (member_potezi nova_pozicija (cdr lista_potega))))
 		))

;Funkcija koja proverava da li je polje iz liste mogucih poteza jednako navedenoj poziciji. Parametri su polje na tabli u vidu liste lista1 i zeljeni potez u vidu liste lista2
(defun proveri_poteg (lista1 lista2)
	(if (equalp (car lista1) (car lista2)) (if (equalp (cadr lista1) (cadr lista2)) t  '() ) '())
	)


;Funkcija povuci_potez (stara nova) vrši pomeranje figure sa pozicije(polja) stara na poziciju nova. Na novoj poziciji se vrši postavljanje figure igrača koji je trenutno na potezu, a na staroj se označava da to polje ostaje prazno. Nakon ovoga se vrši promena igrača.	
(defun povuci_potez (stara nova)
 	(setf (nth 2 ( nth (- (cadr stara ) 1) (nth (- (car stara) 1) tabla))) '0)
 	(setf (nth 2 ( nth (- (cadr nova ) 1) (nth (- (car nova) 1) tabla))) (if (equalp player '0) '1 '2))
 	(setq player (mod (+ player 1) 2))
 		)

;Provera da li imamo pobednika, probojem ili tako sto je igrac pojeo sve protivnicke figure (testiranje ciljnog stanja).
(defun proveri_kraj()
       (cond ((drugi_pojeo_sve) t)
        ((prvi_pojeo_sve) t)
        (t (proboj tabla) (cond ((equalp kraj '1) (format t "Prvi igrac (X) je probio. Pobeda!~%"))
          ((equalp kraj '2) (format t "Drugi igrac (O) je probio. Pobeda!~%"))
          (t (format t "Igra se nastavlja.~%"))) )  
      ))

;Provera da li je drugi igrac pojeo sve figure protivnika. Postavlja indikator nasao na je ako postoji barem jedna protivnicka figura i vraca false ukoliko nije pronadjena ni jedna protivnicka figura.
(defun drugi_pojeo_sve ()
   (nema_figura tabla '1)    ;proverava da li ima X, ako nema kraj = 2
     (cond ( (equalp kraj '2) (format T "Pobedio drugi igrac (O). Pojeo sve figure prvog igraca (X).~%") (setq nasao '0 ) t) 
       (t (setq nasao '0) '()))
    )

;Provera da li je prvi igrac pojeo sve figure protivnika. Postavlja indikator nasao na je ako postoji barem jedna protivnicka figura i vraca false ukoliko nije pronadjena ni jedna protivnicka figura.
(defun prvi_pojeo_sve ()
  (nema_figura tabla '2)
  (cond ((equalp kraj '1) (format T "Pobedio prvi igrac (X). Pojeo sve figure drugog igraca (O).~%") (setq nasao '0 )  t)
    (t  (setq nasao '0) '() ) 
  ))

;Provera da li su sve figure navedenog igraca pojedene. 
(defun nema_figura(lista igrac) 
    (cond((null lista) (cond ((equalp igrac '1) (setq kraj '2) ) ((equalp igrac '2) (setq kraj '1) )))
      ((/= nasao '0) (cond ((equalp nasao '1) (format t "ima jos figura X.~%") (setq nasao '0 )) ((equalp nasao '2) (format t "ima jos figura O.~%") (setq nasao '0 )))) 
    (t (proveri_vrstu (car lista) igrac) (nema_figura (cdr lista) igrac))
         ))

;Funkcija koja proverava da li je doslo do proboja, postavlja indikator kraj na odgovarajucu vrednost, u zavisnosti od toga ko je pobedio.

(defun proboj (lista) ;proverava da li je neki igrac stigao do kraja table
   (proveri_vrstu (car lista) '2) ;da bi doslo do proboja u prvoj vrsti treba da se nadje O (2)
    (if (equalp nasao '2) (setq kraj '2) (proveri_vrstu (car (reverse lista)) '1))  ;ako je postavljen indikator nasao imamo proboj, ako nije proveravamo poslednju vrstu
    (if (equalp nasao '1)  (setq kraj '1))  ;ako je nasao=1 u poslednjoj vrsti imamo X - pobeda prvog igraca
	)
;Provera da li ima protivnickih figura u navedenoj vrsti. Postavlja indikator nasao na 0 ako nema, ili na vrednost 1 ili 2 ako ima. 
(defun proveri_vrstu (lista igrac)    ;prolazi kroz vrstu i trazi figure prosledjenog igraca
      (cond ((null lista)  (setq nasao '0) )  ;stigao do kraja vrste, nema trazenih figura, nije kraj igre -nasao=0
          ((equalp (car (reverse (car lista))) igrac)  (setq nasao igrac)) ;nadjena trazena figura, kraj igre -nasao=IGRAC
          (t (proveri_vrstu (cdr lista) igrac))    ;idemo dalje kroz vrstu
          ))

;Funkcija konvertuj_broj-slovo (s) vrsi konvertovanje broja s u neko od slova od A do Z.
(defun konvertuj_broj-slovo (broj)
  ( case broj
    (1 'A)(2 'B)(3 'C)(4 'D)(5 'E)(6 'F)(7 'G)(8 'H)(9 'I)(10 'J)(11 'K)(12 'L)(13 'M)
    (14 'N)(15 'O)(16 'P)(17 'Q)(18 'R)(19 'S)(20 'T)(21 'U)(22 'V)(23 'W)(24 'X)(25 'Y)(26 'Z)
    ))

;Funkcija konvertuj_slovo-broj (s) vrsi konvertovanje slova s u neki od brojeva od 1 do 26. Obe ove funkcije su potrebne kod unosa poteza.
(defun konvertuj_slovo-broj (slovo)
  (case slovo
              ('A 1)('B 2)('C 3)('D 4)('E 5)('F 6)('G 7)('H 8)('I 9)('J 10)('K 11)('L 12)('M 13)
              ('N 14)('O 15)('P 16)('Q 17)('R 18)('S 19) ('T 20)('U 21)('V 22)('W 23)('X 24)('Y 25)('Z 26)
              ))

;----------------------------------------------------------------------------------------------------------------------------------------------------------------
;                                                                       Zadatak 2
;----------------------------------------------------------------------------------------------------------------------------------------------------------------
;
;                                        Funkcije definisane u drugom delu zadatka koje pozivaju funkcije iz prvog zadatka.
;                                        Fukcije za obezbedjivanje naizmenicnog povlacenja poteza i obezbedjivanje povlacenja validnih poteza,
;                                        definisane su i predate uz prvi zadatak, stoga se nalaze u prvom delu ovog fajla.
;
;----------------------------------------------------------------------------------------------------------------------------------------------------------------

;Funkcija koja kreira listu svih mogucih stanja, prolazeci kroz tablu, vrsta po vrstu i dodavanjem mogucih stanja u listu, za svaku figuru treuntog igraca koja se nalazi u vrsti.
(defun sva_moguca (lis)
  (cond ((null lis) '())
    (t (append (sva_moguca_vrsta (car lis)) (sva_moguca (cdr lis)) )))
  )

;Funkcija koja prolazi kroz prosledjenu vrstu, trazi figure i za pronadjenu figuru u listu mogucih stanja vezanu za prosledjenu vrstu ubacuje sva moguca stanja figure.
(defun sva_moguca_vrsta (row)
  (cond  ((null row) '())
   (t (append (sva_moguca_element (car row)) (sva_moguca_vrsta (cdr row)))))
    )

;Funkcija koja za prosledjeno polje proverava da li se ciljana figura nalazi na tom polju i ako je pronadjena figura ciljana figura, kreira listu njegovih mogucih stanja.
(defun sva_moguca_element (cell)
  (cond ((not (equalp (car (reverse cell))  (+ player 1))) '())   
   (t (setq svi_potezi (moguci_potezi_pomocna (list (first cell) (second cell)))) (append (stanja_figura (list (first cell) (second cell)) svi_potezi) '()))
    ))

;Funkcija koja kreira nova stanja za prosledjenu poziciju na tabli.
(defun stanja_figura (s_poz n_poz)
  (cond ((null n_poz) '())
    (t (append (list(novo_stanje s_poz (car n_poz))) (stanja_figura s_poz (cdr n_poz))))))

;Funkcija koja kreira novo stanje povlacenjem poteza na pomocnoj tabli.
(defun novo_stanje (stara_pozicija nova_pozicija)
  (setq l1 (kopiraj tabla ))
  (povuci_potez_pomocna stara_pozicija nova_pozicija l1)
  l1)

;Pomocna funkcija koja povlaci potez.
(defun povuci_potez_pomocna(stara nova nova_tabla)
  (setf (nth 2 ( nth (- (cadr stara ) 1) (nth (- (car stara) 1) nova_tabla))) '0)
  (setf (nth 2 ( nth (- (cadr nova ) 1) (nth (- (car nova) 1) nova_tabla))) (if (equalp player '0) '1 '2))
  )


;Funkcija koja kreira listu mogucih poteza za prosledjenu poziciju
(defun moguci_potezi_pomocna (stara)
  (cond ((equalp player '0) (remove '() (list (proveri_element_pomocna (+ (car stara) '1 ) (-(cadr stara) '1) '0 '0) (proveri_element_pomocna (+ (car stara) '1) (cadr stara)  '0 '1)  
    (proveri_element_pomocna (+ (car stara) '1) (+(cadr stara) '1) '0 '0))))
  ((equalp player '1) (remove '() (list  (proveri_element_pomocna (- (car stara) '1 ) (-(cadr stara) '1) '1 '0) (proveri_element_pomocna (- (car stara) '1) (cadr stara)  '1 '1)  
    (proveri_element_pomocna (- (car stara) '1) (+(cadr stara) '1) '1 '0))  ))
  ))

;Funkcija za proveru moguceg poteza, da li pijun moze da se pomeri napred ili ukoso.
(defun proveri_element_pomocna  (el1 el2 el3 pravo-koso )
  (cond ((> el1 m) '()) 
    (t (if (equalp pravo-koso '1)
     (if (equalp (car(reverse(nth (- el2 1) (nth (- el1 1) tabla)))) '0) (list  el1  el2));than
      (if (equalp pravo-koso '0) (cond ((equalp el2 0) '())           ;else
            ((equalp el2 (+ n 1))  '())
            (t (if (equalp (+ el3 1) (car(reverse(nth (- el2 1) (nth (- el1 1) tabla))))) '() (list  el1 el2) ))))))))

;Naredne dve funkcije sluze za stampanje kreirane liste mogucih poteza.
(defun stam ()
                (setq l (sva_moguca tabla))
                    (stampaj_sve l))
(defun stampaj_sve (lista)
                (cond ((null lista) '())
                     (t (stampaj (car lista)) (stampaj_sve (cdr lista)))))

;Funkcije za kopiranje liste
(defun kopiraj (l1)
 (cond ((null l1) '())
 (t (append (list (kopiraj_vrstu (car l1))) (kopiraj (cdr l1)))))) 

(defun kopiraj_vrstu (row)
  (cond ((null row)   '())
    (t (append (list (kopiraj_element (car row))) (kopiraj_vrstu (cdr row))) )
  ))

(defun kopiraj_element (cell)
  (cond ((null cell) '())
    (t (append (list (car cell)) (kopiraj_element (cdr cell))))
    )) 

;----------------------------------------------------------------------------------------------------------------------------------------------------------------
;                                                                   Zadatak 3
;----------------------------------------------------------------------------------------------------------------------------------------------------------------
;
;                                        Funkcije za implementaciju min-max algoritma sa alfa-beta odsecanjem
;
;----------------------------------------------------------------------------------------------------------------------------------------------------------------

(defun igra-max(lp dubina a b igrac stanje)
    (cond
        ((null lp) (list stanje a))
        ( t 
      (let* (   (min_stanje (minimax (car lp) (1- dubina) a b (if  (equalp igrac '1) '0 '1))   )
      (na (apply 'max (list a (cadr min_stanje))))
       (novostanje (if (> na a)  (car lp) stanje)))
        (if (< na b) (igra-max (cdr lp) dubina na b igrac novostanje) (list novostanje na))))))        


(defun igra-min(lp dubina a b igrac stanje)
    (cond
        ((null lp) (list stanje b))
        ( t 
      (let* ((max_stanje (minimax (car lp) (1- dubina) a b (if  (equalp igrac '1) '0 '1)))
      (nb (apply 'min (list b (cadr max_stanje))))
       (novostanje (if (< nb b) (car lp) stanje)))
              (if  (> nb a) (igra-min(cdr lp) dubina a nb igrac novostanje) (list novostanje nb) )))))         



(defun minimax (stanje dubina a b igrac)
    (cond
      ((zerop dubina)  (list stanje (heuristika stanje igrac)))
    (t   
      ( setq lp (sva_moguca  stanje))
      (cond 
        ((null lp)  (list stanje (heuristika stanje igrac)))
        (t  (if (equal igrac '1) (igra-max lp dubina a b igrac '()) (igra-min lp dubina a b igrac '())))))))


;----------------------------------------------------------------------------------------------------------------------------------------------------------------
;                                                                       Zadatak 4
;----------------------------------------------------------------------------------------------------------------------------------------------------------------

;pravi bazu cinjenica u vidu asocijativne liste
(defun bajo (board)
  (cond ((null board) '())
    (t (append (bajo_vrsta (car board)) (bajo (cdr board))))))
(defun bajo_vrsta (board)
  (cond ((null board) '())
    (t (append (bajo_element (car board)) (bajo_vrsta (cdr board))))))
(defun bajo_element (cell)
  (cond ((null cell) '())
        (t (cond ((equalp (car(reverse cell))  1) (list 'X (first cell) (second cell)))
                  ((equalp (car(reverse cell))  2) (list 'O (first cell) (second cell)))
                  (t '())))))

(defparameter *T1-RULES*
  '(
    (if (and (X ?a ?b)  (!eq ?a 1)) then (pocetna_X1))
    (if (and (X ?a ?b)  (!eq ?a 2)) then (pocetna_X2))
    (if (and (O ?a ?b)  (!eq ?a m)) then (pocetna_O1))
    (if (and (O ?a ?b)  (!eq ?a (- m 1))) then (pocetna_O2))
    (if (and (X ?a ?b) (!eq ?a m)) then (krajnja_X) )
    (if (and (O ?a ?b) (!eq ?a 1)) then (krajnja_O) )
    (if (and (X ?a ?b) (X ?c ?d) (!eq (+ 1 ?a) ?c) (!eq ?b ?d)) then (blokiran_svoj_X))
    (if (and (X ?a ?b) (O ?c ?d) (!eq (+ 1 ?a) ?c) (!eq ?b ?d)) then (blokiran_tudji_X))
    (if (and (O ?a ?b) (O ?c ?d) (!eq (- ?a 1) ?c) (!eq ?b ?d)) then (blokiran_svoj_O))
    (if (and (O ?a ?b) (X ?c ?d) (!eq (- ?a 1) ?c) (!eq ?b ?d)) then (blokiran_tudji_O))
    (if (and (X ?a ?b) (X ?c ?d) (and (!eq ?c (+ ?a 1)) (!eq ?d (- ?b 1)))) then (blokiran_koso_X1))
    (if (and (X ?a ?b) (X ?c ?d) (and (!eq ?c (+ ?a 1)) (!eq ?d (+ ?b 1)))) then (blokiran_koso_X2))
    (if (and (O ?a ?b) (O ?c ?d) (and (!eq ?c (- ?a 1)) (!eq ?d (- ?b 1)))) then (blokiran_koso_O1))
    (if (and (O ?a ?b) (O ?c ?d) (and (!eq ?c (- ?a 1)) (!eq ?d (+ ?b 1)))) then (blokiran_koso_O2))
    (if (and (X ?a ?b) (X ?c ?d) (and (!eq ?a (+ ?c 1)) (!eq ?b (+ 1 ?d)))) then (branjen_X1)) 
    (if (and (X ?a ?b) (X ?c ?d) (and (!eq ?a (+ ?c 1)) (!eq ?b (- ?d 1)))) then (branjen_X2)) 
    (if (and (O ?a ?b) (O ?c ?d) (and (!eq ?a (- ?c 1)) (!eq ?b (+ 1 ?d)))) then (branjen_O1)) 
    (if (and (O ?a ?b) (O ?c ?d) (and (!eq ?a (- ?c 1)) (!eq ?b (- ?d 1)))) then (branjen_O2)) 
    (if (and (X ?a ?b) (O ?c ?d) (and (!eq ?a (- ?c 1)) (!eq ?b (+ ?d 1)))) then (napadac_X1))
    (if (and (X ?a ?b) (O ?c ?d) (and (!eq ?a (- ?c 1)) (!eq ?b (- ?d 1)))) then (napadac_X2))
    (if (and (O ?a ?b) (X ?c ?d) (and (!eq ?a (+ ?c 1)) (!eq ?b (+ ?d 1)))) then (napadac_O1))
    (if (and (O ?a ?b) (X ?c ?d) (and (!eq ?a (+ ?c 1)) (!eq ?b (- ?d 1)))) then (napadac_O2))
    (if (and (X ?a ?b) (O ?c ?d) (and (!eq ?c (+ ?a 1)) (!eq ?d (- ?b 1)))) then  (jede_X1))
    (if (and (X ?a ?b) (O ?c ?d) (and (!eq ?c (+ ?a 1)) (!eq ?d (+ ?b 1)))) then  (jede_X2))
    (if (and (O ?a ?b) (X ?c ?d) (and (!eq ?c (- ?a 1)) (!eq ?d (- ?b 1)))) then  (jede_O1))
    (if (and (O ?a ?b) (X ?c ?d) (and (!eq ?c (- ?a 1)) (!eq ?d (+ ?b 1)))) then  (jede_O2))
    (if (X ?a ?b) then (figura_X))
    (if (O ?a ?b) then (figura_O))
    ))

;Pravila se koriste da bi odredili koliko figura ispunjava navedeni uslov. Neka pravila su duplirana zbog toga sto u masini ne radi OR operacija.
;Na kraju svaki rezultat operacije count-results mnozimo odredjenim koeficijentom i sve proizvode sumiramo, da bi dobili vrednost heuristike.
(defun heuristika (board igrac)
 (setq *T1-FACTS* (bajo board))
 (prepare-knowledge *T1-RULES* *T1-FACTS* 5)
 (cond ((equalp igrac '0) 
   (cond ((/= (count-results '(krajnja_X)) 0) '1000)
    (t  (+ (* (count-results '(figura_X)) 100) (* (count-results '(figura_O)) -100) (- (+ (* (count-results '(pocetna_O1)) 10) (* (count-results '(pocetna_O2)) 20) (* (count-results '(blokiran_svoj_O)) -20) (* (count-results '(blokiran_tudji_O)) -10)
                (* (count-results '(blokiran_koso_O1)) -20) (* (count-results '(blokiran_koso_O2)) -20) (* (count-results '(branjen_O1)) 40) (* (count-results '(branjen_O2)) 40)
                (* (count-results '(napadac_O1)) -20)  (* (count-results '(napadac_O2)) -20) (* (count-results '(jede_O1)) 20) (* (count-results '(jede_O2)) 20))
            (+ (* (count-results '(pocetna_X1)) 10) (* (count-results '(pocetna_X2)) 20) (* (count-results '(blokiran_svoj_X)) -20) (* (count-results '(blokiran_tudji_X)) -10)
                (* (count-results '(blokiran_koso_X1)) -20) (* (count-results '(blokiran_koso_X2)) -20) (* (count-results '(branjen_X1)) 40) (* (count-results '(branjen_X2)) 40)
                (* (count-results '(napadac_X1)) -20)  (* (count-results '(napadac_X2)) -20) (* (count-results '(jede_X1)) 1000) (* (count-results '(jede_X2)) 1000)))))
    ))
        ((equalp igrac '1) 
          (cond ((/= (count-results '(krajnja_X)) 0) -1000)
    (t  (+ (* (count-results '(figura_X)) -100) (* (count-results '(figura_O)) 100)(- (+ (* (count-results '(pocetna_X1)) 10) (* (count-results '(pocetna_X2)) 20) (* (count-results '(blokiran_svoj_X)) -20) (* (count-results '(blokiran_tudji_X)) -10)
              (* (count-results '(blokiran_koso_X1)) -20) (* (count-results '(blokiran_koso_X2)) -20) (* (count-results '(branjen_X1)) 40) (* (count-results '(branjen_X2)) 40)
              (* (count-results '(napadac_X1)) -20)  (* (count-results '(napadac_X2)) -20) (* (count-results '(jede_X1)) 20) (* (count-results '(jede_X2)) 20)) 
            (+ (* (count-results '(pocetna_O1)) 10) (* (count-results '(pocetna_O2)) 20) (* (count-results '(blokiran_svoj_O)) -20) (* (count-results '(blokiran_tudji_O)) -10)
                (* (count-results '(blokiran_koso_O1)) -20) (* (count-results '(blokiran_koso_O2)) -20) (* (count-results '(branjen_O1)) 40) (* (count-results '(branjen_O2)) 40)
                (* (count-results '(napadac_O1)) -20)  (* (count-results '(napadac_O2)) -20) (* (count-results '(jede_O1)) 1000) (* (count-results '(jede_O2)) 1000)))))
    )
          )
  )
)


; PREDEFINISANI PREDIKAT

(defun !eq (a b)
  (equal a b))