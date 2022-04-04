#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))





; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.


; Inainte de totul, am pus o conditie ca cei doi vectori sa aiba aceeasi lungime,
; desi se garanteaza ca este adevarat.
;
; Am verificat la inceputul recursivitatii, pentru a avea conditia de oprire
; a adaugarii elementelor pe stiva, prin a verifica daca X sau Y mai au
; elemente. In acest caz, am returnat 0, iar daca acestea mai au elemente,
; atunci, se inmultesc primele elemente intre ele si se aduna
; cu rezultatul apelarii pe restul de elemente din vectori.

(define (dot-product X Y)
  (if (or (null? X) (null? Y))
     0
     (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)))))





; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.


; Pentru recursivitatea pe coada am folosit o functie auxiliara
; pentru a o putea apela. In aceasta functie multiply-helper am
; returnat F in cazul in care s-a terminat matricea si in rest
; am apelat multiply-helper pe urmatoarea linie din M, pe vectorul V
; si am adaugat la lista finala F valoarea lui dot-product dintre
; linia curenta din matrice si vectorul V.
;
; Am apelat multiply-helper din functia multiply cu M V si lista finala
; F fiind lista vida

(define (multiply-helper M V F)
  (if (null? M)
      F
      (multiply-helper (cdr M) V (append F (list (dot-product (car M) V))))))

(define (multiply M V)
  (multiply-helper M V '()))






; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)


; Pentru aceasta functie am folosit recursivitatea pe coada
; initial apelata pentru n si lista vida. Pe urma la fiecare
; pas pentru care n este inca diferit de 1, am apelat functia
; get-tranformation-helper pe urmatorul nivel de mai sus
; adica pentru (n + 1) / 3 si am adaugat la finalul listei
; indexul matricei folosit.
;
; Daca luam exemplul dat, se observa ca pentru fiecare element
; cel din stanga trebuie imnultit cu T1, cel din mijloc cu T2 iar
; cel din dreapta cu T3
;;
;;                         (3,4,5)(1)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)(2)   (21,20,29)(3)  (5,12,13)(4)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37)(5) ..........................................
;;
; Deci spre exemplu pentru elementul 5, acesta trebuie imnultit cu T1, dupa
; trecem la tatal sau si acela trebuie la randul lui inmultit cu T1, deci
; rezultatul final va fi '(1 1), iar pentru 6 va fi '(2 1).
;
; La final, cand n devine 1 returnam reverse de lista finala pentru ca append-ul
; va adauga elemente la finalul listei.


(define (get-transformations-helper n F)
  (if (equal? n 1)
      (reverse F)
      (get-transformations-helper (quotient (+ n 1) 3) (append F (list (+ (modulo (+ n 1) 3) 1))))))

(define (get-transformations n)
  (get-transformations-helper n '()))





; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.


; Folosind recursivitatea pe coada am implementat functia
; apply-matrix-transformation-helper in care am verificat
; daca am ajuns la finalul listei Ts, iar daca da am returnat
; tripletul F final. In cazul in care mai sun elemente in lista
; am verificat daca elementul curect este 1,2 sau 3 iar pentru
; fiecare am inmultit cu matricea aferenta si am apelat iar functia helper.
; cu noile date.
;
; Pentru functia apply-matrix-transformation doar am apelat helper cu datele date.


(define (apply-matrix-transformations-helper Ts F)
  (if (null? Ts)
      F
      (cond [(equal? (car Ts) 1) (apply-matrix-transformations-helper (cdr Ts) (multiply T1 F))]
            [(equal? (car Ts) 2) (apply-matrix-transformations-helper (cdr Ts) (multiply T2 F))]
            [(equal? (car Ts) 3) (apply-matrix-transformations-helper (cdr Ts) (multiply T3 F))])))

(define (apply-matrix-transformations Ts ppt)
  (apply-matrix-transformations-helper Ts ppt))







; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.

; Pentru aceasta functie am folosit functiile apply-matrix-transformations
; pentru a obtine lista Ts de indecsi, iar pe urma aplic pentru ppt = (3 4 5), lista
; de inceput de la care pornim pentru TPP.


(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5)))








