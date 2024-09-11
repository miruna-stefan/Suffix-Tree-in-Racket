#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")

(provide (all-defined-out))

;; În această etapă definim algoritmul de construcție a unui
;; arbore de sufixe (atât cel compact, cât și cel atomic) pe
;; baza unui text și a unui alfabet dat. Se știe că textul
;; utilizează doar simboluri prezente în alfabet.
;; Abordarea este următoarea:
;; 1. obținem toate sufixele textului
;; 2. pentru fiecare caracter din alfabet, determinăm sufixele
;;    care încep cu acel caracter: ele vor fi grupate în câte
;;    o ramură a arborelui de sufixe (exceptând caracterele
;;    care nu apar in text - acestea nu generează ramuri)
;; 3. pentru fiecare listă de sufixe care încep cu un același
;;    caracter, determinăm eticheta ramurii, respectiv noile
;;    sufixe care vor genera subarborele de sub etichetă
;;    - în cazul unui AST (atomic suffix tree), eticheta este
;;      chiar primul caracter, iar noile sufixe sunt vechile
;;      sufixe fără primul caracter
;;    - în cazul unui CST (compact suffix tree), eticheta este
;;      cel mai lung prefix comun al sufixelor, iar sufixele
;;      noi se obțin din cele vechi prin îndepărtarea acestui
;;      prefix
;; 4. transformăm fiecare rezultat de la pasul 3 într-o ramură
;;    - eticheta este deja calculată
;;    - calculul subarborilor se realizează repetând pașii 2-4
;;      pentru noile sufixe


; TODO 1
; Implementați recursiv o funcție care primește un text (listă 
; de caractere) și determină lista tuturor sufixelor acestuia
; (de la cel mai lung la cel mai scurt).
; Se știe că textul se va termina cu caracterul special "$", 
; și cu acest caracter trebuie să se termine și toate sufixele
; din lista rezultat (de la întreg textul urmat de "$" până la
; sufixul vid - reprezentat de un singur "$").
; ex:
; (get-suffixes '(#\w #\h #\y #\$))
; => '((#\w #\h #\y #\$) (#\h #\y #\$) (#\y #\$) (#\$))
; Folosiți recursivitate pe stivă.

;; function that returns a list of charcaters, which is actually the longest suffix (the word in itself)
;; basically this function's role is to transform the string text into a list of chars
(define (get-suffix text)
  (if (null? text)
      '()
      (cons (car text) (get-suffix (cdr text)))))


;; returns a list of all suffixes of the text
(define (get-suffixes text)
  (if (null? text)
      '()
      (append (list(get-suffix text)) (get-suffixes (cdr text)))))


; TODO 2
; Implementați o funcție care primește o listă de cuvinte 
; și un caracter ch și întoarce acele cuvinte din listă care 
; încep cu caracterul ch.
; Atenție, este posibil ca unele cuvinte să fie vide.
; Folosiți funcționale (și nu folosiți recursivitate explicită).

;; transformed the starts-with? uncurried function into a curried one
(define (curry-starts-with? ch)
  (lambda (word)
    (starts-with? word ch)))

;; function that returns true if the text starts with the given character ch and false otherwise
(define (starts-with? word ch)
  (cond
    ((null? word) #f)
    ((equal? (car word) ch) #t)
    (else #f)))

;; returns a list of all the words starting with charcater ch
(define (get-ch-words words ch)
  (if (null? words)
      '()
      (filter (curry-starts-with? ch) words))
  )


; TODO 3
; Implementați o funcție care primește o listă nevidă de sufixe 
; care încep cu același caracter și calculează perechea
; (etichetă AST pentru aceste sufixe, lista noilor sufixe).
; Reamintim că pentru un AST eticheta este chiar primul caracter
; (dar sub formă de listă de caractere, pentru a putea fi 
; procesată la fel ca etichetele mai complexe), iar noile sufixe 
; se obțin din cele vechi prin eliminarea acestui caracter.
; Nu folosiți recursivitate explicită.
(define (ast-func suffixes)
  (if (null? suffixes)
      '()
      (cons (list (caar suffixes)) (map (lambda (x) (cdr x)) suffixes))))


; TODO 4
; Implementați o funcție care primește o listă nevidă de sufixe 
; care încep cu același caracter și calculează perechea
; (etichetă CST pentru aceste sufixe, lista noilor sufixe).
; Reamintim că pentru un CST eticheta este cel mai lung prefix
; comun, iar noile sufixe se obțin din cele vechi prin eliminarea 
; acestui prefix.
; Nu folosiți recursivitate explicită.

;; curry function that removes the first n characters from a list of chars
(define (curry-substring n)
  (lambda (text)
    (drop text n)))

(define (cst-func suffixes)
  (if (null? suffixes)
      '()
      (let* ((common-prefix (longest-common-prefix-of-list suffixes)))
        ;; pair the longest common prefix of all suffixes with the list of the rests
        ;; of the suffixes (after removing the common prefix from them)
        (cons common-prefix (map (curry-substring (length common-prefix)) suffixes)))
      ))


; TODO 5
; Implementați funcția suffixes->st care construiește un
; arbore de sufixe pe baza unei liste de sufixe, a unui 
; alfabet (listă de caractere care include toate caracterele
; din sufixe), și a unei funcții care indică modul de
; etichetare (atomic sau compact).
; Când argumentul funcție va fi ast-func, vom obține un AST.
; Când argumentul funcție va fi cst-func, vom obține un CST.
; Obs: Funcția surprinde pașii 2-4 descriși mai sus.
; Funcția suffixes->st poate fi recursivă explicit, dar
; pentru celelalte prelucrări (pașii 2 și 3) trebuie să
; folosiți funcționale.

(define (suffixes->st labeling-func suffixes alphabet)
  (if (null? suffixes)
      empty-st
      ;; parse all letters in the alphabet from left to right (the alphabet is already
      ;; sorted, so we will get the letters in alphabetical order)
      (foldl (lambda (letter acc) ;; acc will be the suffix tree
               (let* ((suffixes-starting-with-letter (get-ch-words suffixes letter)))
                 ;; check if there are any suffixes starting with the current letter
                 (if (null? suffixes-starting-with-letter)
                     acc
                     ; store the result of applying the labeling function on the suffixes that start with the current letter
                     (let* ((paired-suffixes-label (labeling-func suffixes-starting-with-letter))
                            ; create a new branch by associating the correspondent label with the subtree
                            ; then call this function recursively for the subtree
                            (new-branch (append (list (get-branch-label paired-suffixes-label))
                                                (suffixes->st labeling-func (get-branch-subtree paired-suffixes-label) alphabet))))
                       ; add the new branch to the whole tree
                       (append acc (list new-branch)))
                       )))
             empty-st
             alphabet)))

; TODO 6
; Această sarcină constă în implementarea a trei funcții:
; text->st, text->ast, text->cst, unde text->ast și text->cst
; trebuie obținute ca aplicații parțiale ale lui text->st.
; În acest scop, funcția text->st trebuie să fie curry.

; a) Implementați funcția text->st care primește un text
; (listă de caractere) și o funcție de etichetare și întoarce
; arborele de sufixe corespunzând acestui text cu această
; metodă de etichetare.
; Pași:
; - obținem sufixele textului la care adăugăm marcajul final $
; - obținem alfabetul sortat asociat textului prin utilizarea
;   funcțiilor de bibliotecă sort, remove-duplicates, char<?
;   (inclusiv caracterul $)
; - apelăm corespunzător funcția suffixes->st
; Vă veți defini singuri parametrii funcției text->st și ordinea 
; acestora, dar funcția trebuie să fie curry într-un mod care 
; facilitează derivarea funcțiilor text->ast și text->cst de 
; mai jos prin aplicație parțială a funcției text->st.
; Obs: Din acest motiv, checker-ul testează doar funcțiile
; text->ast și text->cst.

;; helper function that adds the #\$ terminator to the text
(define (add-dollar-sign-to-text text)
  (reverse (cons '#\$ (reverse text))))

;; helper function that returns the alphabet by applying library functions on the complete text
(define (get-alphabet text)
  (sort (remove-duplicates (add-dollar-sign-to-text text)) char<?))

;; helper function that returns the list of suffixes correspondent to the complete text
;; (with dollar sign at the end)
(define (text->suffixes text)
  (get-suffixes (add-dollar-sign-to-text text)))

(define (text->st labeling-func)
  (lambda (text)
      (suffixes->st labeling-func (text->suffixes text) (get-alphabet text))))


; b) Din funcția text->st derivați funcția text->ast care
; primește un text (listă de caractere) și întoarce AST-ul
; asociat textului.
(define text->ast
    (text->st ast-func))

; c) Din funcția text->st derivați funcția text->cst care
; primește un text (listă de caractere) și întoarce CST-ul
; asociat textului.
(define text->cst
    (text->st cst-func))
