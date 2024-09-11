#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
  (if (null? text)
      #f
      (let ((suffix-tree (text->cst text))) ; transform the text into a suffix tree
    (st-has-pattern? suffix-tree pattern)))
  )


; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).

(define (longest-common-substring text1 text2)
  (let ((st1 (text->cst text1)) ; transform the first text into a suffix tree
        (suffix-list2 (get-suffixes text2))) ; get the list of suffixes from the second text
    
    ; use a named let to parse the suffixes list of the second text
    (let parse-suffixes ((suffixes suffix-list2) (longest-substring '()))
      ; check if we have reached the end of the suffix list
      (if (null? suffixes)
          longest-substring
          ; for each suffix in the list, look for the longest substring that matches text1
          ; firstly, find the longest substring of the current suffix that matches text1
          ; then, compare its length with the substring that has had the maximum length until now
          (let ((current-substring (let search-substring ((curr-st st1) (current-suffix (car suffixes))) ; use another named let to parse through the substrings of the current suffix
                                     (let ((match-result (match-pattern-with-label curr-st current-suffix)))  
                                       (cond
                                            ((eq? match-result #t) current-suffix) ; this is the maximum match (the label contains the entire pattern)
                                            ((eq? (car match-result) #f) (cadr match-result)) ; the label and the pattern do not match until the end => get the longest common prefix
                                            (else ; the pattern contains the entire label => look further for the pattern in the rest of the tree
                                             ; use a let to properly identify the terms in the list that resulted from the match-pattern-with-label function
                                             ; match-result is the following type of list: (label remaining-pattern new-subtree)
                                             (let* ((label (car match-result))
                                                    (new-pattern-and-subtree (cdr match-result))
                                                    (new-subtree (car (cdr new-pattern-and-subtree)))
                                                    (new-pattern (car new-pattern-and-subtree)))
                                               (append label (search-substring new-subtree new-pattern)))
                                             )
                                            )
                                          )
                                        )))
            ; if the substring obtained for the current suffix is longer than the maximum, update maximum
            (if (> (length current-substring) (length longest-substring))
                (parse-suffixes (cdr suffixes) current-substring)
                (parse-suffixes (cdr suffixes) longest-substring))
        ))
      )
    )
  )


; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.

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
      ((eq? (length current-branch) 1) (find-substring rest-of-branches substring-length))
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
