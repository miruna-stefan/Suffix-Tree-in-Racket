#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

; remains unmodified because it just operates on words, which remain lists of characters
(define (longest-common-prefix-helper w1 w2 acc)
  (cond
    ((null? w1) (list acc w1 w2))
    ((null? w2) (list acc w1 w2))
    ((equal? (car w1) (car w2)) (longest-common-prefix-helper (cdr w1) (cdr w2) (append acc (list (car w1)))))
    (else (list acc w1 w2))))

(define (longest-common-prefix w1 w2)
  (longest-common-prefix-helper w1 w2 '()))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection


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
(define (longest-common-prefix-of-collection-helper prefix words)
  (if (collection-empty? words)
      prefix
      (longest-common-prefix-of-collection-helper (just-the-longest-common-prefix prefix (collection-first words)) (collection-rest words))))


(define (longest-common-prefix-of-collection words)
  (cond
    ; if the word collection is empty, the prefix is an empty list
    ((collection-empty? words) '())
    ; if the collection only contains one word, this will be the entire prefix
    ((collection-empty? (collection-rest words)) (collection-first words))
    ; if the collection only consists of 2 words, the prefix for the whole collection can be calculated using the function dedicated for two words
    ((collection-empty? (collection-rest (collection-rest words))) (just-the-longest-common-prefix (collection-first words) (collection-second words)))
    ; the collection contains more than 2 words
    (else (longest-common-prefix-of-collection-helper (just-the-longest-common-prefix (collection-first words) (collection-second words)) (collection-rest (collection-rest words))))))


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
  (if (not (get-ch-branch st (car pattern)))
      (list '#f '()) ;; the pattern does not appear in text
      (existing-branch (get-ch-branch st (car pattern)) pattern))) ;; found correspondent branch


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


;; returns a collection of all suffixes of the text
(define (get-suffixes text)
  (if (null? text)
      empty-collection
      (collection-cons text (get-suffixes (cdr text)))))


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

;; returns a collection of all the words starting with charcater ch
(define (get-ch-words words ch)
  (if (collection-empty? words)
      empty-collection
      (collection-filter (curry-starts-with? ch) words))
  )


(define (ast-func suffixes)
  (if (collection-empty? suffixes)
      empty-collection
      (cons (list (car (collection-first suffixes))) (collection-map (lambda (x) (cdr x)) suffixes))))


;; curry function that removes the first n characters from a list of chars
(define (curry-substring n)
  (lambda (text)
    (drop text n)))

(define (cst-func suffixes)
  (if (collection-empty? suffixes)
      empty-collection
      (let ((common-prefix (longest-common-prefix-of-collection suffixes)))
        ;; pair the longest common prefix of all suffixes with the list of the rests
        ;; of the suffixes (after removing the common prefix from them)
        (cons common-prefix (collection-map (curry-substring (length common-prefix)) suffixes)))
      ))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)


;; helper function that generates a list of branches, each one starting with a different letter in the alphabet
;; basically, we will have an entry in the branches list for each letter in the alphabet
(define (generate-all-branches labeling-func suffixes alphabet)
  ;; parse all letters in the alphabet (the alphabet is already
  ;; sorted, so we will get the letters in alphabetical order)
  (collection-map (lambda(letter)
         (let ((suffixes-starting-with-letter (get-ch-words suffixes letter)))
           ;; check if there are any suffixes starting with the current letter
           (if (collection-empty? suffixes-starting-with-letter)
               '()
               ; store the result of applying the labeling function on the suffixes that start with the current letter
               (let* ((paired-suffixes-label (labeling-func suffixes-starting-with-letter))
                      ; store the current label and the subtree based on the paired-suffixes-label result
                      (current-label (get-branch-label paired-suffixes-label))
                      (subtree (get-branch-subtree paired-suffixes-label)))
                 ; create a new branch by associating the correspondent label with the subtree
                 ; then call this function recursively for the subtree
                 (cons current-label (suffixes->st labeling-func subtree alphabet))
                 ))))
       alphabet))

;; helper function dedicated to "cleaning up" the empty branches
(define (only-keep-useful-branches all-branches)
  (collection-filter (lambda (branch) (not (null? branch))) all-branches))

(define (suffixes->st labeling-func suffixes alphabet)
  (if (collection-empty? suffixes)
      empty-st
      ; we need to keep in the ST only the branches that are not null, so we need to filter
      ; the result of the generate-branches function, which stores a list of branches, whose
      ; length will be equal to the number of charactrs in the alphabet
      (only-keep-useful-branches (generate-all-branches labeling-func suffixes alphabet))))


; nu uitați să convertiți alfabetul într-un flux


;; helper function that adds the #\$ terminator to the text
(define (add-dollar-sign-to-text text)
  (reverse (cons '#\$ (reverse text))))

;; helper function that returns the alphabet by applying library functions on the complete text
(define (get-alphabet text)
  (sort (remove-duplicates (add-dollar-sign-to-text text)) char<?))

;; helper function that converts a list into a collection
(define (list->collection L)
  (if (null? L)
      empty-collection
      (collection-cons (car L) (list->collection (cdr L)))))

;; helper function that returns the list of suffixes correspondent to the complete text
;; (with dollar sign at the end)
(define (text->suffixes text)
  (get-suffixes (add-dollar-sign-to-text text)))

(define (text->st labeling-func)
  (lambda (text)
      (suffixes->st labeling-func (text->suffixes text) (list->collection (get-alphabet text)))))


(define text->ast
    (text->st ast-func))


(define text->cst
    (text->st cst-func))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.

(define (substring? text pattern)
  (if (null? text)
      #f
      (let ((suffix-tree (text->ast text))) ; transform the text into a suffix tree
    (st-has-pattern? suffix-tree pattern)))
  )


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.


(define (repeated-substring-of-given-length text len)
  ; create a named let to browse through the st
  (let find-substring ((st (text->cst text)) (substring-length len))
    (let ((current-branch (first-branch st))
          (rest-of-branches (other-branches st)))
      (cond
      ; check if the suffix tree is empty
      ((st-empty? st) #f)
      ; if the current branch does not have at least 2 nodes => there is no intern node (node with children) in this branch
      ; => move on to the rest of the st's branches
      ((collection-empty? (cdr current-branch)) (find-substring rest-of-branches substring-length))
      ; if we reached this point, it means that the branch has more than one node (meaning that there is at least one node with children)
      (else
       (let* ((current-label (get-branch-label current-branch))
              (label-length (length current-label)))
         (cond
           ; check if the current label has exactly the given length
           ((equal? substring-length label-length) current-label)
           ; if the label is longer than len, keep only the first len characters
           ((> label-length substring-length) (take current-label substring-length))
           ; if the label is shorter than len, look deeper in the subtree to complete the missing characters up until len
           (else (let* ((remaining-length (- substring-length label-length))
                        (complete-result (find-substring (get-branch-subtree current-branch) remaining-length)))
                   ; check if the recursive call of the named let on the subtree returned false or a list of characters
                   (cond
                     ; we have found the rest of the characters, so we need to add them to the label to form the complete result
                     ((not (eq? complete-result #f)) (append current-label complete-result))
                     ; if we reached this point, it means that we could not find the rest of the characters up until len in the
                     ; subtree either => investigate the rest of the branches
                     (else (find-substring rest-of-branches substring-length)))
                   ))
           )
         ))
      ))
    ))