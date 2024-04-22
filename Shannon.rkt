#lang htdp/isl+
(require 2htdp/image)
(require 2htdp/universe)


;;;;;;;;;;;;;;;;;;;;;;

;; Previous Changes ;;

;;;;;;;;;;;;;;;;;;;;;;

; summary of changes:
; parse-cell input format description
; grid-temp? 
; parse-cell: edited conditionals to handle interesting cases (ie if there is no first s)
; parse-grid: takes in SExp instead of ListOf [ListOf SExp]
; parse-grid: used built in abstraction using map instead
; gate-helper: redefined with abstraction 
; all-assigned signature, statement, tests
; assign-and/or: redefined 

; Exercise 1
; A cell is a (make-cell [Mixed wire gate string] string) 
; where the contents of the cell is one of: Input or Output wire; Gate - (AND, OR, NOT) and orientation; Conductive plate; Empty
; and the assigned is one of: "none" "pos" "neg", representing the assignment of the cell (initally set as none)
; Interpretation: a cell represents a square in a grid of Shannon


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cell/Grid Definition Cell Functions ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct cell (content assigned))
(define ACell (signature (predicate cell?)))
(define CELL-SIZE 30)
(define Image (signature (predicate image?)))
(define Posn (signature (predicate posn?)))

; InOrOut
(define WireType (signature (enum "input" "output"))) 
(define-struct wire [value type]) ; value is variable 
(define InOrOut (signature [WireOf String WireType]))
(define (wire-temp w)
  (... (wire-value w) ... (wire-type w) ...))

;Gate
(define Operator (signature (enum "and" "or" "not")))
(define Orientation (signature (enum "up" "down" "left" "right")))
(define-struct gate [operator orientation])
(define AGate (signature [GateOf Operator Orientation]))
(define (gate-temp g)
  (... (gate-operator g) ... (gate-orientation g) ...))

; Examples:
(define c1 (make-cell (make-wire "X" "input") "none"))
(define c2 (make-cell (make-wire "Y" "input") "none"))
(define c3 (make-cell (make-wire "Z" "output") "none"))
(define c4 (make-cell "plate" "none"))
(define c5 (make-cell (make-gate "not" "down") "none"))
(define ce (make-cell "empty" "none"))

; Template:
(define (cell-temp c)
  (cond [(string=? (cell-content c) "plate") ...]
        [(string=? (cell-content c) "empty") ...]
        [(wire? (cell-content c)) (wire-temp c)]
        [(gate? (cell-content c)) (gate-temp c)]
        [(string=? (cell-assigned c) "none") ...]
        [(string=? (cell-assigned c) "pos") ...]
        [(string=? (cell-assigned c) "neg") ...]))

; a Grid is a [ListOf [ListOf ACell]]
; which contains a list inside a list of cells representing the each row 
(define AGrid (signature [ListOf [ListOf ACell]]))
(define (grid-temp g)
  (... (cond [(empty? g)...]
             [(empty? (first g))...(rest g)...]
             [(cons? g)]
             [(cons? (first g))...(cell-temp (first (first g)))...(grid-temp (rest (first g)))])))

(define G1 (list (list c1 c2) (list c3 ce)))
(define G2 (list (list c5 ce c1) (list ce ce ce) (list c4 c2 c5)))

(: cell->image (ACell -> Image))
;turns a cell into the image of the cell
(define (cell->image cell)
  (cond[(string? (cell-content cell)) (cond[(string=? "empty" (cell-content cell)) (square CELL-SIZE "solid" "gray")]
                            [(string=? "plate" (cell-content cell)) (overlay/align "middle" "middle" (text "c" 23 "black") (square CELL-SIZE "solid" "gray"))])]
       [(gate? (cell-content cell)) (overlay/align "middle" "middle"
                                    (text (string-append (gate-operator (cell-content cell)) " \n " (gate-orientation (cell-content cell))) 10 "black")
                                    (square CELL-SIZE "solid" "gray"))]
       [(wire? (cell-content cell))(cond [(equal? "input" (wire-type (cell-content cell)))
                           (overlay/align "middle" "middle"
                                          (text (string-append "i" (wire-value (cell-content cell))) 23 "black")
                                          (square CELL-SIZE "solid" "gray"))]
                          [(equal? "output" (wire-type (cell-content cell)))
                           (overlay/align "middle" "middle"
                                          (text (string-append "o" (wire-value (cell-content cell))) 23 "black")
                                          (square CELL-SIZE "solid" "gray"))])]))

(check-expect (cell->image ce) (square CELL-SIZE "solid" "gray"))

(: cell->color (ACell -> Image))
;returns a red cell if cell is assigned as neg, green if assigned as pos, and gray if unassigned
(define (cell->color c)
  (cond [(string=? (cell-assigned c) "pos") (overlay (square CELL-SIZE 50 "green") (cell->image c))]
        [(string=? (cell-assigned c) "neg") (overlay (square CELL-SIZE 50 "red") (cell->image c))]
        [else (cell->image c)]))

(check-expect (cell->color c1) (cell->image c1))
(check-expect (cell->color (make-cell "empty" "pos")) (overlay (square CELL-SIZE 50 "green") (cell->image ce)))

(: row->image ([ListOf ACell] -> Image))
;turns a row from the grid into an image
(define (row->image g)
  (cond [(empty? g) (square CELL-SIZE 0 "black")] ;last member of current row
        [(cons? g) (beside/align "bottom"
                                 (cell->color (first g))
                                 (row->image (rest g)))])) ; random member of first row

(: grid->image (AGrid -> Image))
;takes in a grid and displays the image of the grid
(define (grid->image g)
  (cond [(empty? (rest g)) (row->image (first g))]
        [(cons? g) (above/align "right"
                               (row->image (first g))
                               (grid->image (rest g)))]))

(check-expect (grid->image G1) (above/align "right" (row->image (first G1)) (row->image (first (rest G1)))))

(: cell-set-equal? ([ListOf ACell] [ListOf ACell] -> Boolean))
;checks if the two list have the same amount of cells of each type
(define (cell-set-equal? l1 l2)
  (cond [(and (empty? l1) (empty? l2)) #t]
        [(not (equal? (length l1) (length l2)))#f]
        [else (cell-set-equal? (remove (first l1) l1) (remove (first l1) l2))]))

(check-expect (cell-set-equal? (list c1 c2) (list c2 c1)) #t)
(check-expect (cell-set-equal? [list ce ce c4] [list ce c4]) #f)
(check-expect (cell-set-equal? [list c1 c5] [list c1 c2]) #f)

(: get-cell (AGrid Posn -> (mixed ACell Boolean)))
;returns the cell at a given position that's on the grid.
;note: both empty cell and cell out of bound returns #false
(define (get-cell g p)
  (cond [(or (> (+ 1 (posn-y p)) (length g))(> (+ 1(posn-x p)) (length (first g)))) #f] 
        [(or (< (posn-y p)0)(< (posn-x p)0)) #f]
        [else (list-ref (list-ref g (posn-y p)) (posn-x p))]))

(check-expect (get-cell G1 (make-posn 0 0)) c1)

(: set-row ([ListOf ACell] Number ACell -> [ListOf ACell]))
;set-row takes a row and a posn-x and a cell
(define (set-row l x cell)
  (cond [(> 0 x) l]
        [(= 0 x) (cons cell (set-row (rest l) (- x 1) cell))]
        [(cons? l) (cons (first l) (set-row (rest l) (- x 1) cell))]))

(check-expect (set-row (first G1) 0 c2) (list c2 c2)) 
                  
(: set-cell (AGrid Posn ACell -> (mixed AGrid Boolean)))
;takes in a cell and a position on a grid and returns the grid with the new cell replacing the old one at that posn
(define (set-cell g p c)
  (cond [(empty? g) '()]
        [(or (> (posn-x p) (length (first g))) (> (posn-y p) (length g))) #f]
        [(= 0 (posn-y p)) (cons (set-row (first g) (posn-x p) c) (rest g))]
        [(cons? g) (cons (first g) (set-cell (rest g)
                                             (make-posn (posn-x p) (- (posn-y p) 1)) c))]))
(check-expect (set-cell G1 (make-posn 0 0) ce) (list (list ce c2) (list c3 ce)))
(check-expect (set-cell G1 (make-posn 20 20) ce) #f)

(: get-neighbors (AGrid Posn -> [ListOf (mixed Boolean ACell)]))
;return a list of cell that neighbors the cell of requested position
(define (get-neighbors g p)
  (local [(define N (make-posn (posn-x p) (+ (posn-y p)1)))
          (define S (make-posn (posn-x p) (- (posn-y p)1)))
          (define W (make-posn (-(posn-x p)1) (posn-y p)))
          (define E (make-posn (+ (posn-x p) 1) (posn-y p)))]
    (cons (get-cell g E)(cons (get-cell g W)(cons (get-cell g S)(cons (get-cell g N) '()))))))

(check-expect (get-neighbors G2 (make-posn 0 0))(list ce #false #false ce))


;;;;;;;;;;;

;; SEXPs ;;

;;;;;;;;;;;



(define SExp (signature (mixed Symbol String [ListOf SExp])))
; An SExp is one of: 
; Symbol
; List of Sexp
(define S1 "hello")
(define S2 '(hello "there" ((1 2) 3) #t))
(define S3 '(1 (2 (3))))

; Exercise 2:
(: parse-cell (SExp -> ACell))
; takes in an S Expression where cell types are separated by parentheses & returns ACell
(define (parse-cell s)
  (cond [(symbol? s)
         (cond [(symbol=? s 'E) (make-cell "empty" "none")]
               [(symbol=? s 'P) (make-cell "plate" "none")])]
        [(and (cons? s) (equal? (length s) 3))
         (cond [(and (symbol? (first s)) (symbol=? (first s) 'W))
                 (make-cell (make-wire (first (rest s)) (first (rest (rest s)))) "none")]
                [(and (symbol? (first s)) (symbol=? (first s) 'G))
                 (make-cell (make-gate (first (rest s)) (first (rest (rest s)))) "none")])]
         [else (error "Could not parse " s)]))

(check-expect (parse-cell '(W "X" "input")) (make-cell (make-wire "X" "input") "none"))
(check-expect (parse-cell '(G "not" "up")) (make-cell (make-gate "not" "up") "none"))
(check-expect (parse-cell 'E) (make-cell "empty" "none"))
(check-expect (parse-cell 'P) (make-cell "plate" "none"))
(check-error (parse-cell 'Y)) 
(check-error (parse-cell '()))

(: make-row ([ListOf SExp] -> [ListOf ACell]))
; takes in a list of S Expressions and returns a row of cells
(define (make-row g)
  (cond [(empty? g) '()]
        [(and (cons? g)
              (or (equal? (first g) 'W)
                  (equal? (first g) 'G)))
         (cons (parse-cell (cons (first g)
                                 (list (first (rest g))
                                       (first (rest (rest g))))))
               (make-row (rest (rest (rest g)))))]
        [(cons? g) (cons (parse-cell (first g)) (make-row (rest g)))]
        [else (error "Invalid cell row " g)]))

(check-expect (make-row (first PG1))
              (list (make-cell (make-wire "X" "input") "none")
                    (make-cell (make-gate "not" "up") "none")
                    (make-cell "empty" "none")))
(check-expect (make-row (second PG2))
              (list (make-cell "plate" "none")
                    (make-cell (make-wire "Z" "Output") "none")))

(: parse-grid (SExp -> AGrid))
; takes in a list of a list of expressions and returns a grid
(define (parse-grid g)
  (cond [(empty? g) '()]
        [(cons? g) (map make-row g)]
        [else (error "Invalid grid input " g)]))

; examples & tests
(define PG1 '((W "X" "input" G "not" "up" E)
              (E E P)
              (P E W "Y" "Output")))
(define PG2 '((E G "or" "right")
              (P W "Z" "Output")))

(check-expect (parse-grid PG1)
              (list (list (make-cell (make-wire "X" "input") "none") (make-cell (make-gate "not" "up") "none") (make-cell "empty" "none"))
                    (list (make-cell "empty" "none") (make-cell "empty" "none") (make-cell "plate" "none"))
                    (list (make-cell "plate" "none") (make-cell "empty" "none") (make-cell (make-wire "Y" "Output") "none"))))
(check-expect (parse-grid PG2)
              (list (list (make-cell "empty" "none") (make-cell (make-gate "or" "right") "none"))
                    (list (make-cell "plate" "none") (make-cell (make-wire "Z" "Output") "none"))))

(define G3 (list (list (make-cell (make-wire "X" "input") "pos") c4) (list c4 c4)))
(define G4 (list (list (make-cell (make-gate "or" "right") "none") ce)
                 (list (make-cell (make-wire "X" "input") "pos") ce)))
(define G5 (list (list (make-cell (make-gate "and" "right") "none") ce)
                 (list (make-cell (make-wire "X" "input") "pos") ce)))


;;;;;;;;;;;;;;;;;;;;;;;;

;; Inital Update Grid ;;

;;;;;;;;;;;;;;;;;;;;;;;;



(: check-neighbors ([ListOf (mixed Boolean ACell) ] -> String))
;entered a list of neighbors checks their assigments and returns the first assignment found
(define (check-neighbors lon)
  (cond [(empty? lon) "none"]
        [(equal? #f (first lon)) (check-neighbors (rest lon))]
        [(cons? lon) (cond [(string=? (cell-assigned (first lon)) "pos") "pos"]
                           [(string=? (cell-assigned (first lon)) "neg") "neg"]
                           [(string=? (cell-assigned (first lon)) "none") (check-neighbors (rest lon))])]))

(check-expect (check-neighbors (list #f c1 c2 (make-cell "plate" "pos") (make-cell "plate" "neg")))"pos")
(check-expect (check-neighbors (list #f c2 c1 (make-cell "plate" "neg") (make-cell "plate" "neg")))"neg")

(: assign-not (ACell [ListOf (mixed ACell Boolean)] -> ACell))
;takes in a cell and list of its neighbors and assigns the opposite of the first neighbor it sees
(define (assign-not cell row) 
  (cond [(string=? (cell-assigned cell) "none")
         (cond [(string=? (check-neighbors row) "pos") (make-cell (cell-content cell) "neg")]
               [(string=? (check-neighbors row) "neg") (make-cell (cell-content cell) "pos")]
               [(string=? (check-neighbors row) "none") (make-cell (cell-content cell) "none")])]
        [else cell]))

(check-expect (assign-not (first (first G1)) (get-neighbors G1 (make-posn 0 0))) (make-cell (make-wire "X" "input") "none"))
(check-expect (assign-not (first (second G1)) (get-neighbors G1 (make-posn 1 0))) (make-cell (make-wire "Z" "output") "none"))

(: same-assign? ([ListOf (mixed ACell Boolean)] String -> Boolean))
;takes in a list of cells; returns #t if all assignments (not including empty cells) have the same assignment as the string and #f otherwise
(define (same-assign? l s) 
  (cond [(empty? l) #t]
        [(equal? #f (first l)) (same-assign? (rest l) s)];out of bound case
        [(and (string? (cell-content (first l)))
              (string=? (cell-content (first l)) "empty")) (same-assign? (rest l) s)];empty case
        [(not (string=? (cell-assigned (first l)) s)) #f]
        [else (same-assign? (rest l) s)]))

(check-expect (same-assign? (list c1 c2) "none") #t)
(check-expect (same-assign? (list (make-cell "plate" "pos") (make-cell "plate" "neg")) "pos") #f)
(check-expect (same-assign? (list (make-cell "plate" "neg") (make-cell "plate" "neg")) "neg") #t)

(: gate-helper (ACell [ListOf (mixed ACell Boolean)] String String -> ACell))
;a helper function for assigning gate, it takes in a cell, and a list of cells
;if one neighbor is assigned to s1, assign cell as s1, if all neighbors are s2, assign cell s2
;or: s1 = pos, s2 = neg; vise versa for and
(define (gate-helper cell l s1 s2)
  (cond[(ormap (lambda (x) (and (not (equal? #f x)) (string=? (cell-assigned x) s1))) l)
        (make-cell (cell-content cell) s1)]
       [(same-assign? l s2) (make-cell (cell-content cell) s2)]
       [else cell]))

(check-expect (gate-helper (first (first G1)) (get-neighbors G1 (make-posn 0 0)) "pos" "neg") (make-cell (make-wire "X" "input") "none"))
(check-expect (gate-helper (first (second G1)) (get-neighbors G1 (make-posn 1 0)) "neg" "pos") (make-cell (make-wire "Z" "output") "none"))

(: assign-and/or (ACell AGrid Posn String String -> ACell))
;takes in a gate and if the cell isnt assigned yet, checks the direction and the neighbors not in the direction
;assigns s1 if at least 1 neighbor is s1, and s2 if all 3 neighbors are s2
(define (assign-and/or cell g pos s1 s2)
  (cond [(not (= (length (get-neighbors g pos)) 4)) (error "wrong length")]
        [(string=? (cell-assigned cell) "none")
         (cond[(string=? (gate-orientation (cell-content cell)) "right")
               (gate-helper cell (rest (get-neighbors g pos)) s1 s2)]
              [(string=? (gate-orientation(cell-content cell)) "left")
               (gate-helper cell (remove (second (get-neighbors g pos)) (get-neighbors g pos)) s1 s2)]
              [(string=? (gate-orientation(cell-content cell)) "up")
               (gate-helper cell (remove (third (get-neighbors g pos)) (get-neighbors g pos)) s1 s2)]
              [(string=? (gate-orientation(cell-content cell)) "down")
               (gate-helper cell (remove (fourth (get-neighbors g pos)) (get-neighbors g pos)) s1 s2)])]
        [else cell]))

(check-expect (assign-and/or (first (first G4)) G4 (make-posn 0 0) "pos" "neg") (make-cell (make-gate "or" "right") "pos"))
(check-error  (assign-and/or (first (first G1)) G1 (make-posn 0 0) "pos" "neg"))

(: all-assigned? ([ListOf (mixed ACell Boolean)] -> Boolean))
; takes in a list of cells and #f (if cell doesn't exist) and returns whether all are assigned
(define (all-assigned? l)
  (cond [(empty? l) #t]
        [(or (equal? (first l) #f)
             (and (string? (cell-content (first l))) (string=? (cell-content (first l)) "empty"))
             (not (string=? (cell-assigned (first l)) "none")))
         (all-assigned? (rest l))]
        [else #f]))

(check-expect (all-assigned? (list c1 c2)) #f)
(check-expect (all-assigned? (list #f c1)) #f)
(check-expect (all-assigned? (list (make-cell "plate" "pos") (make-cell "plate" "neg"))) #t)

(: assign-row ([ListOf ACell] AGrid Posn -> [ListOf ACell]))
;iterates through the cell and the next row 
(define (assign-row loc g p)
  (cond [(empty? loc) '()] ;empty row
        [(and (string? (cell-content (first loc))) 
              (string=? "plate" (cell-content (first loc)))
              (string=? "none" (cell-assigned (first loc)))) 
         (cond [(string=? (check-neighbors (get-neighbors g p)) "pos")
                (cons (make-cell (cell-content (first loc)) "pos")
                      (assign-row (rest loc) g (make-posn (+ 1 (posn-x p)) (posn-y p))))]
               [(string=? (check-neighbors (get-neighbors g p)) "neg")
                (cons (make-cell (cell-content (first loc)) "neg")
                      (assign-row (rest loc) g (make-posn (+ 1 (posn-x p)) (posn-y p))))]
               [(string=? (check-neighbors (get-neighbors g p)) "none")
                (cons (make-cell (cell-content (first loc)) "none")
                      (assign-row (rest loc) g (make-posn (+ 1 (posn-x p)) (posn-y p))))])] ;plate
        [(gate? (cell-content (first loc)))
         (cond [(string=? (gate-operator (cell-content (first loc))) "not")
                (cons (assign-not (first loc) (get-neighbors g p))
                      (assign-row (rest loc) g (make-posn (+ 1 (posn-x p)) (posn-y p))))]
               [(string=? (gate-operator (cell-content (first loc))) "or")
                (cons (assign-and/or (first loc) g p "pos" "neg")
                      (assign-row (rest loc) g (make-posn (+ 1 (posn-x p)) (posn-y p))))]
               [(string=? (gate-operator (cell-content (first loc))) "and")
                (cons (assign-and/or (first loc) g p "neg" "pos")
                      (assign-row (rest loc) g (make-posn (+ 1 (posn-x p)) (posn-y p))))])];gate
        [(and (wire? (cell-content (first loc))) 
              (string=? (wire-type (cell-content (first loc))) "output")
              (string=? (cell-assigned (first loc)) "none")
              (all-assigned? (get-neighbors g p)))
         (cond [(same-assign? (get-neighbors g p) "pos")
                (cons (make-cell (cell-content (first loc)) "pos") (assign-row (rest loc) g (make-posn (+ 1 (posn-x p)) (posn-y p))))]
               [(same-assign? (get-neighbors g p) "neg")
                (cons (make-cell (cell-content (first loc)) "neg") (assign-row (rest loc) g (make-posn (+ 1 (posn-x p)) (posn-y p))))]
               [else (error "contradiction")])];wire output 
        [else (cons (first loc) (assign-row (rest loc) g (make-posn (+ 1 (posn-x p)) (posn-y p))))]))         

(check-expect (assign-row (first G1) G1 (make-posn 0 0)) (list (make-cell (make-wire "X" "input") "none") (make-cell (make-wire "Y" "input") "none")))
(check-expect (assign-row (list (make-cell (make-wire "X" "input") "pos") (make-cell "plate" "pos") (make-cell (make-gate "not" "up") "none"))
                          (list (list (make-cell (make-wire "X" "input") "pos") (make-cell "plate" "pos") (make-cell (make-gate "not" "up") "none"))) (make-posn 0 0))
              (list (make-cell (make-wire "X" "input") "pos") (make-cell "plate" "pos") (make-cell (make-gate "not" "up" ) "neg")))
(check-expect (assign-row (first G4) G4 (make-posn 0 0)) (list(make-cell(make-gate "or" "right") "pos")(make-cell "empty" "none")))
(check-expect (assign-row (first G5) G5 (make-posn 0 0)) (list(make-cell (make-gate "and" "right") "pos") (make-cell "empty" "none")))

(: gate-dir-helper (AGrid Posn Posn -> AGrid))
;helper function for gate-dir, if logic works, sets a cell otherwise it returns an error 
(define (gate-dir-helper g po pn)
  (cond [(not (cell? (get-cell g pn))) g]
        [(cell? (get-cell g pn)) 
                (cond [(gate? (cell-content (get-cell g pn))) g]
                      [(string=? (cell-assigned (get-cell g pn)) "none")
                       (set-cell g pn (make-cell (cell-content (get-cell g pn)) (cell-assigned (get-cell g po))))]
                      [(string=? (cell-assigned (get-cell g pn)) (cell-assigned (get-cell g po))) g]
                      [else (error "contradiction")])]))

(check-error (gate-dir-helper G4 (make-posn 0 0) (make-posn 0 1)))
(check-expect (gate-dir-helper G4 (make-posn 0 0) (make-posn 0 0)) G4)
         
(: gate-dir ([ListOf ACell] AGrid Posn -> AGrid))
;goes through the row and returns a grid following the gates
(define (gate-dir loc g p)
  (cond [(empty? loc) g]
        [(and (gate? (cell-content (first loc))) (not (equal? (cell-assigned (first loc)) "none")))
         (cond [(string=? (gate-orientation (cell-content (first loc))) "right")
                (gate-dir (rest loc)
                          (gate-dir-helper g p (make-posn (+ 1 (posn-x p))(posn-y p)))
                          (make-posn (+ (posn-x p) 1) (posn-y p)))]
               [(string=? (gate-orientation (cell-content (first loc))) "left")
                (gate-dir (rest loc)
                          (gate-dir-helper g p (make-posn (- (posn-x p) 1)(posn-y p)))
                          (make-posn (+ (posn-x p) 1) (posn-y p)))]
               [(string=? (gate-orientation (cell-content (first loc))) "up")
                (gate-dir (rest loc)
                          (gate-dir-helper g p (make-posn (posn-x p) (- (posn-y p) 1)))
                          (make-posn (+ (posn-x p) 1) (posn-y p)))]
               [(string=? (gate-orientation (cell-content (first loc))) "down")
                (gate-dir (rest loc)
                          (gate-dir-helper g p (make-posn  (posn-x p) (+ 1(posn-y p))))
                                           (make-posn (+ (posn-x p) 1) (posn-y p)))])]
        [else (gate-dir (rest loc) g (make-posn (+ (posn-x p) 1) (posn-y p)))]))

(check-expect (gate-dir (first G4) G4 (make-posn 0 0))
              (list (list (make-cell (make-gate "or" "right") "none") (make-cell "empty" "none"))
                    (list (make-cell (make-wire "X" "input") "pos") (make-cell "empty" "none"))))
(check-expect (gate-dir (first G1) G1 (make-posn 0 0)) G1)

(: gate-dir-all [AGrid AGrid Posn -> AGrid])
;assigns the cell that the gate points towards (will give an error if it there is a contradiction)
(define (gate-dir-all g1 g2 p)
  (cond [(empty? g1) g2]
        [(cons? (first g1))
         (gate-dir-all (rest g1)
                       (gate-dir (first g1) g2 p)
                       (make-posn (posn-x p) (+ 1 (posn-y p))))]))

(check-expect (gate-dir-all G6 G6 (make-posn 0 0)) G6)
              
(define G6 (parse-grid '((G "not" "right" P W "Z" "output") (P E P) (W "X" "input" P G "not" "up"))))

(: assign-grid (AGrid AGrid Posn -> AGrid))
; goes through a grid and implements it wihtout considering the gates for now 
(define (assign-grid g1 g2 p)
  (cond [(empty? g1) '()]
        [else (cons (assign-row (first g1) g2 p)(assign-grid (rest g1) g2 (make-posn (posn-x p) (+ 1 (posn-y p)))))]))

(check-expect (assign-grid G1 G1 (make-posn 0 0)) (list (list (make-cell (make-wire "X" "input") "none")
                                                              (make-cell (make-wire "Y" "input") "none"))
                                                        (list (make-cell (make-wire "Z" "output") "none")
                                                              (make-cell "empty" "none"))))
(check-expect (assign-grid G3 G3 (make-posn 0 0)) (list (list (make-cell (make-wire "X" "input") "pos")
                                                              (make-cell "plate" "pos"))
                                                        (list (make-cell "plate" "pos") (make-cell "plate" "none"))))

(: update-grid (AGrid -> AGrid))
;updates the grid based on all factors  
(define (update-grid g)
  (gate-dir-all (assign-grid g g (make-posn 0 0)) (assign-grid g g (make-posn 0 0)) (make-posn 0 0)))

(check-expect (update-grid G1) (list (list (make-cell (make-wire "X" "input") "none")
                                           (make-cell (make-wire "Y" "input") "none"))
                                     (list (make-cell (make-wire "Z" "output") "none")
                                           (make-cell "empty" "none"))))

(define G7 (parse-grid '((P W "A" "input" G "or" "down" W "B" "input" P P P)
                         (P E P E E E P)
                         (P E G "and" "right" P W "Z" "output" E P)
                         (P E G "not" "up" E E E P)
                         (P P G "and" "up" P P P P))))
(define G8 (parse-grid '((P W "A" "input" P P P P P)
                         (P E P E E E P)
                         (P E G "and" "right" P W "Z" "output" E P)
                         (P E G "not" "up" E E E P)
                         (P P G "and" "up" P P P P))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Introductions and implementation of Goal ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct goal (inputs outputs simulated?))
;a goal is a (make-goal (ListOf Wire) (ListOf Wire)) Boolean
;inputs is the desired input wires, and the outputs are the desired output wires and if the goal has already been simulated
(define AGoal (signature (predicate goal?)))

(define GOAL1 (make-goal (list (parse-cell '(W "X" "input"))) (list (parse-cell '(W "Z" "output"))) #f))
(define GOAL2 (make-goal (list (make-cell (make-wire "X" "input") "pos")) (list (make-cell (make-wire "Z" "output") "neg")) #f))
(define GOAL3 (make-goal (list (make-cell (make-wire "X" "input") "neg")) (list (make-cell (make-wire "Z" "output") "pos")) #f))
(define GOAL4 (make-goal (list (make-cell (make-wire "X" "input") "neg")) (list (make-cell (make-wire "Z" "output") "pos")) #t))
(define GOAL5 (make-goal (list (make-cell (make-wire "X" "input") "neg")) (list (make-cell (make-wire "Z" "output") "pos")) #t))

(: goal-reached? (AGoal AGrid -> Boolean))
;takes in a goal and a grid, checkes on the grid if both the input and output conditions are reached
(define (goal-reached? goal grid)
  (local ([define (search-row l c) ;search-row : [ListOf ACell] ACell -> Boolean
            (cond [(empty? l) #f]
                  [(and (wire? (cell-content (first l)))
                        (string=? (wire-value (cell-content (first l))) (wire-value (cell-content c)))
                        (string=? (cell-assigned c) (cell-assigned (first l)))) #t]
                  [else (search-row (rest l) c)])]
          [define (search-grid g c) ;search-grid : AGrid ACell -> Boolean
            (cond [(empty? g) #f]
                  [(search-row (first g) c) #t]
                  [else (search-grid (rest g) c)])]
          [define (search l g) ;search : (ListOf ACell) AGrid -> Boolean
            (cond [(empty? l) #t]
                  [(search-grid g (first l)) (search (rest l) g)]
                  [else #f])])
    (and (search (goal-inputs goal) grid) (search (goal-outputs goal) grid))))

(check-expect (goal-reached? GOAL1 (parse-grid '((W "X" "input" P) (P W "Z" "output")))) #t)
(check-expect (goal-reached? GOAL2 (parse-grid '((W "X" "input" P) (P W "Z" "output")))) #f)

(: assign-input (AGrid AGoal -> AGrid))
; takes in a grid & a goal and assigns all inputs from the goal into the grid 
(define (assign-input grid goal)
  (local [(define (assign-input-row l c) ;assign-input-row : (ListOf ACell) ACell -> (ListOf ACell)
            (cond [(empty? l) '()]
                  [(and (wire? (cell-content (first l)))
                        (string=? (wire-value (cell-content (first l))) (wire-value (cell-content c)))
                        (string=? (wire-type (cell-content (first l))) "input"))
                   (cons c (assign-input-row (rest l) c))]
                  [else (cons (first l) (assign-input-row (rest l) c))]))
          (define (assign-input-grid g c) ;assign-input-grid : AGrid ACell -> AGrid
            (cond [(empty? g) '()]
                  [else (cons (assign-input-row (first g) c) (assign-input-grid (rest g) c))]))]
    (cond [(empty? (goal-inputs goal)) grid]
          [(cons? (goal-inputs goal))
           (assign-input (assign-input-grid grid (first (goal-inputs goal))) (make-goal (rest (goal-inputs goal)) (goal-outputs goal) #f))])))

(check-expect (assign-input (parse-grid '((W "X" "input" P) (P W "Z" "output"))) GOAL2)
(list (list (make-cell (make-wire "X" "input") "pos") (make-cell "plate" "none"))
      (list (make-cell "plate" "none") (make-cell (make-wire "Z" "output") "none"))))

(define-struct worldstate (grid goals))
; a WorldState is a make-worldstate AGrid [ListOf AGoal]
(define AWorldState (signature (predicate worldstate?)))
(define WS1 (make-worldstate G6 (list GOAL1 GOAL2)))

(: draw-ws (AWorldState -> Image))
;draws the world state, with the grid on the left side and if the goals are accomplished on the right side
(define (draw-ws ws)
  (overlay (beside/align "bottom"
            (grid->image (worldstate-grid ws))
            (goal->image ws)) (empty-scene 800 500)))

(check-expect (draw-ws WS1) (overlay (beside/align
                                      "bottom"
                                      (grid->image (worldstate-grid WS1))
                                      (goal->image WS1)) (empty-scene 800 500)))

(: goal->image (AWorldState -> Image))
;turns the goal into an image, with all the goals and a checkbox if the goal is met
(define (goal->image ws)
  (local
    [(define (loc->text loc) ;loc->text : [ListOf Cell] -> String ;;turns the goal into a string
       (cond [(empty? loc) ""]
             [(cons? loc)
              (string-append (wire-value (cell-content (first loc)))
                             " is "
                             (cell-assigned (first loc))
                             (loc->text (rest loc)))]))
     (define (checkbox grid goal) ;check-box : AWorldState -> Image ;;solid checkbox if its simulated and outline if not
       (if (goal-simulated? goal)
           (square 10 "solid" "black")
           (square 10 "outline" "black")))]
    (cond [(empty? (worldstate-goals ws)) (square 10 "solid" "white")]
          [(cons? (worldstate-goals ws))
           (above/align
            "left"
            (beside/align
             "middle"
             (checkbox (worldstate-grid ws) (first (worldstate-goals ws)) )
             (text (string-append
                    " If "
                    (loc->text (goal-inputs (first (worldstate-goals ws))))
                    " then "
                    (loc->text (goal-outputs (first (worldstate-goals ws)))))
                   20 "black"))
            (goal->image (make-worldstate (worldstate-grid ws) (rest (worldstate-goals ws)))))])))

(check-expect (goal->image WS1) (above/align
                                 "left"
                                 (beside/align "middle" (square 10 "outline" "black")
                                               (text " If X is none then Z is none" 20 "black"))
                                 (beside/align "middle" (square 10 "outline" "black")
                                               (text " If X is pos then Z is neg" 20 "black"))
                                 (square 10 "solid" "white")))

(: all-simulated? (AWorldState -> Boolean))
;checks if all of the goals in a world state are simulated
(define (all-simulated? ws)
  (andmap (lambda (x) (goal-simulated? x)) (worldstate-goals ws)))

(check-expect (all-simulated? WS1) #f)
(check-expect (all-simulated? (make-worldstate G1 (list GOAL4 GOAL5))) #t)

(: win-screen (AWorldState -> Image))
;overlays the "Win!" ontop of a finished game
(define (win-screen ws)
  (overlay/align "middle" "top"
                 (text "Win!" 40 "black")
                 (draw-ws ws)))

(check-expect (win-screen WS1) (overlay/align "middle" "top"
                                              (text "Win!" 40 "black")
                                              (draw-ws WS1)))
(check-expect (win-screen WS2) (overlay/align "middle" "top"
                                              (text "Win!" 40 "black")
                                              (draw-ws WS2)))

(: update-worldstate (AWorldState -> AWorldState))
;updates the world state on every tick to check if each of the goals are met
(define (update-worldstate ws)
  (local[(define (reset-row loc) ;reset-row : [ListOf Cell] -> [ListOf Cell];;resets the row inside the grid
           (cond [(empty? loc) '()]
                 [else (cons (make-cell (cell-content (first loc)) "none") (reset-row (rest loc)))]))
         (define unreached-goals (filter (lambda (x) (not (goal-simulated? x))) (worldstate-goals ws)))
         (define reached-goals (filter (lambda (x) (goal-simulated? x)) (worldstate-goals ws)))]
    (cond [(empty? unreached-goals) ws]
          [(goal-reached? (first unreached-goals) (worldstate-grid ws))
           (make-worldstate
            (map reset-row (worldstate-grid ws))
            (append reached-goals
                    (list (make-goal (goal-inputs (first unreached-goals))
                                     (goal-outputs (first unreached-goals))
                                     #t))
                    (rest unreached-goals)))]
          [else (make-worldstate
                 (update-grid (assign-input (worldstate-grid ws) (first unreached-goals)))
                 (worldstate-goals ws))])))

(check-expect (update-worldstate WS2) (make-worldstate (update-grid (assign-input G6 GOAL2))
                                                       (list GOAL2 GOAL3)))
(check-expect (update-worldstate WS3) (make-worldstate (update-grid (worldstate-grid WS3))
                                                       (list GOAL2 GOAL3)))

(define WS2 (make-worldstate G6 (list GOAL2 GOAL3)))
(define WS3 (make-worldstate (update-grid (assign-input G6 GOAL2))
                                                       (list GOAL2 GOAL3)))


(: main (AWorldState -> AWorldState))
;main function that runs the game, stops when all goals are reached 
(define (main ws)
  (big-bang ws
    (to-draw draw-ws)
    (on-tick update-worldstate 0.3)
    (stop-when all-simulated? win-screen)))


(define G9 (parse-grid '((W "X" "input" P P P P G "not" "down" P P P W "Y" "input")
                         (P E E E E P E E E P)
                         (P E E P P G "and" "left" P P P P)
                         (P E E P P E E E E P)
                         (G "or" "right" P P E P E E E E P)
                         (E E P G "and" "down" P E P P P P)
                         (E E E P E E P E E P)
                         (E E E P P P G "and" "down" P P P)
                         (E E E E E E P E E E)
                         (E E E E E E W "Z" "output" E E E))))
(define GOALG91 (make-goal (list (make-cell (make-wire "X" "input") "pos") (make-cell (make-wire "Y" "input") "pos"))
                           (list (make-cell (make-wire "Z" "output") "neg")) #f))
(define GOALG92 (make-goal (list (make-cell (make-wire "X" "input") "neg") (make-cell (make-wire "Y" "input") "pos"))
                           (list (make-cell (make-wire "Z" "output") "neg")) #f))
(define GOALG93 (make-goal (list (make-cell (make-wire "X" "input") "pos") (make-cell (make-wire "Y" "input") "neg"))
                           (list (make-cell (make-wire "Z" "output") "neg")) #f))

(define WS4 (make-worldstate G9 (list GOALG91 GOALG92 GOALG93)))

(define G10 (parse-grid '((W "A" "input" P P E E W "B" "input" E E E W "C" "input")
                          (P E P E G "not" "down" P E E E P)
                          (P E P E P E E E E P)
                          (P P G "and" "down" E P P E P P P)
                          (E E P E E P P G "or" "down" E E)
                          (P P P P G "not" "down" E E P E E)
                          (G "not" "down" E E E P E E G "not" "down" E E)
                          (P E P P P P E P E E)
                          (W "X" "output" E G "not" "down" E E P E P E E)
                          (E E P P P G "and" "right" P G "and" "right" W "Y" "output" E))))

(define GOALG101 (make-goal (list (make-cell (make-wire "A" "input") "pos") (make-cell (make-wire "B" "input") "pos") (make-cell (make-wire "C" "input") "pos"))
                           (list (make-cell (make-wire "X" "output") "neg") (make-cell (make-wire "Y" "output") "neg")) #f))
(define GOALG102 (make-goal (list (make-cell (make-wire "A" "input") "neg") (make-cell (make-wire "B" "input") "pos") (make-cell (make-wire "C" "input") "neg"))
                           (list (make-cell (make-wire "X" "output") "pos") (make-cell (make-wire "Y" "output") "neg")) #f))

(define WS5 (make-worldstate G10 (list GOALG101 GOALG102)))