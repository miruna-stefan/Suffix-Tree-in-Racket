#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.

; helper function that returns a list containing the longest common prefix of the 2 words,
; the rest of the first word and the rest of the second word
; acc = longest common prefix
(define (longest-common-prefix-helper w1 w2 acc)
  (cond
    ((null? w1) (list acc w1 w2))
    ((null? w2) (list acc w1 w2))
    ((equal? (car w1) (car w2)) (longest-common-prefix-helper (cdr w1) (cdr w2) (append acc (list (car w1)))))
    (else (list acc w1 w2))))

(define (longest-common-prefix w1 w2)
  (longest-common-prefix-helper w1 w2 '()))


; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.


; helper function inspired by the one implemented for task 2, but this one only returns the prefix, without the rests of the lists
(define (just-the-longest-common-prefix-helper w1 w2 acc)
  (cond
    ((null? w1) acc)
    ((null? w2) acc)
    ((equal? (car w1) (car w2)) (just-the-longest-common-prefix-helper (cdr w1) (cdr w2) (append acc (list (car w1)))))
    (else acc)))

(define (just-the-longest-common-prefix w1 w2)
  (just-the-longest-common-prefix-helper w1 w2 '()))

; helper function that builds the prefix step by step, by calling the function recursively
(define (longest-common-prefix-of-list-helper prefix words)
  (if (null? words)
      prefix
      (longest-common-prefix-of-list-helper (just-the-longest-common-prefix prefix (car words)) (cdr words))))


(define (longest-common-prefix-of-list words)
  (cond
    ; if the list is empty, the prefix is an empty list
    ((null? words) '())
    ; if the list only contains one word, this will be the entire prefix
    ((= (length words) 1) (car words))
    ; if the list only consists of 2 words, the prefix for the whole list can be calculated using the function dedicated for two words
    ((= (length words) 2) (just-the-longest-common-prefix (car words) (cadr words)))
    ; the list contains more than 2 words
    (else (longest-common-prefix-of-list-helper (just-the-longest-common-prefix (car words) (cadr words)) (cddr words)))))


;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.


;; function that checks on which of the 3 cases we are, only after finding a correspondent branch in the subtree
(define (existing-branch branch pattern)
  (let ((label (get-branch-label branch))
        (common-prefix (just-the-longest-common-prefix (get-branch-label branch) pattern)))
    (cond
      ; check if the label contains the entire pattern
      ((equal? common-prefix pattern) #t)
      ; check if the pattern contains the entire label
      ((equal? common-prefix label) (list label (last (longest-common-prefix label pattern)) (get-branch-subtree branch)))
      ; the label and the pattern have a common prefix, but they don't match until the end
      (else (list '#f common-prefix)))))


(define (match-pattern-with-label st pattern)
  ;; check if there is a branch whose label corresponds to the pattern
  (if (not(get-ch-branch st (car pattern)))
      (list '#f '()) ;; the pattern does not appear in text
      (existing-branch (get-ch-branch st (car pattern)) pattern))) ;; found correspondent branch


; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.

(define (st-has-pattern? st pattern)
  ;; store in match-result the return value of the previous function
  ;; the previous function returns #t only if the label contains the whole pattern. Otherwise, it returns a list
  (let ((match-result (match-pattern-with-label st pattern)))
    (if (not (list? match-result))
      #t
      ;; if we reached this point, it means that the previous function returned a list

      ;; if the list starts with the element #f, it is clear that we also need to return #f,
      ;; because the label does not contain the whole pattern
      (if (equal? (car match-result) '#f)
          #f
          ;; if the list starts with element #t, it means that the pattern contains the whole label,
          ;; so we need to check if the subtree contains the rest of the pattern
          (st-has-pattern? (caddr match-result) (cadr match-result))))))