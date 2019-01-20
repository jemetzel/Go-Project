#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require (only-in typed/racket/gui/base put-file get-file))

;; data definitions

(define-struct (Some a)
  ([value : a]))

(define-type (Optional a)
  (U 'None (Some a)))

(define-type Stone
  (U 'black 'white))

(define-struct LogicalLoc
  ([col : Integer]
   [row : Integer]))

(define-type Board
  (Vectorof (Vectorof (Optional Stone))))

(define-struct Go
  ([board : Board]
   [next-to-play : Stone]
   [history : (Listof Board)]
   [last-turn-place : (Optional LogicalLoc)]
   [last-turn-opp-captures : (Listof LogicalLoc)]
   [last-turn-self-captures : (Listof LogicalLoc)]
   [consecutive-passes : Integer]))

(define-struct PhysicalLoc
  ([x-offset-from-left : Integer]
   [y-offset-from-top  : Integer]))

(define-struct BoardSpec
  ([background-color : Image-Color]
   [cell-size-pixels : Integer]
   [margin-pixels : Integer]
   [stone-radius-pixels : Integer]))

(define-struct World
  ([spec : BoardSpec]
   [game : Go]
   [status-message : String]
   [black-tenths : Integer]
   [white-tenths : Integer]
   [hover : (Optional LogicalLoc)]))

(: sample-board : Integer -> Board)
(define (sample-board i)
  (vector
   (vector 'None 'None (Some 'black) (Some 'white))
   (vector (Some 'black) (Some 'black) (Some 'black) 'None)
   (vector 'None (Some 'white) (Some 'white) 'None)
   (vector 'None (Some 'white) 'None (Some 'black))))

(define-struct Outcome
  ([black  : Integer]
   [white  : Integer]
   [winner : (U Stone 'draw)]))

;; self: returns its input
(: self : All (a) a -> a)
(define (self x) x)

(check-expect (self 2) 2)

;; the integer argument is the dimension (locations per side) of the board
(: logical->physical : LogicalLoc Integer BoardSpec -> PhysicalLoc)
(define (logical->physical loc dim board)
  (match (list loc board)
    [(list (LogicalLoc col row) (BoardSpec bcolor csize msize srad))
     (if (or (< col 0) (< row 0) (> col (- dim 1)) (> row (- dim 1)))
         (error "logical->physical: location is not within dimensions of board")
         (PhysicalLoc (+ msize (* csize col))
                      (+ msize (* csize (- dim 1 row)))))]))

(check-expect
 (logical->physical (LogicalLoc 0 2) 4 (BoardSpec 'blue 12 16 4))
 (PhysicalLoc 16 28))
(check-expect
 (logical->physical (LogicalLoc 0 3) 4 (BoardSpec 'blue 12 16 4))
 (PhysicalLoc 16 16))
(check-expect
 (logical->physical (LogicalLoc 29 29) 30 (BoardSpec 'black 10 12 4))
 (PhysicalLoc 302 12))
(check-expect
 (logical->physical (LogicalLoc 0 0) 30 (BoardSpec 'black 10 12 4))
 (PhysicalLoc 12 302))
(check-error
 (logical->physical (LogicalLoc 30 0) 30 (BoardSpec 'black 10 12 4))
 "logical->physical: location is not within dimensions of board")

;; the integer argument is the dimension (locations per side) of the board
(: physical->logical : PhysicalLoc Integer BoardSpec -> (Optional LogicalLoc))
(define (physical->logical loc dim board)
  (match (list loc board)
    [(list (PhysicalLoc x y) (BoardSpec color csize msize srad))
     (local
       {(define nearest-col
          (cond
            [(> 0 (round (/ (- x msize) csize))) 0]
            [(< (- dim 1) (round (/ (- x msize) csize))) (- dim 1)]
            [else (round (/ (- x msize) csize))]))
        (define nearest-row
          (cond
            [(> 0 (round (/ (- y msize) csize))) 0]
            [(< (- dim 1) (round (/ (- y msize) csize))) (- dim 1)]
            [else (round (/ (- y msize) csize))]))}
       (if (> srad
              (sqrt (+ (expt (- x (+ msize (* csize nearest-col))) 2)
                       (expt (- y (+ msize (* csize nearest-row))) 2))))
           (Some (LogicalLoc nearest-col (- dim 1 nearest-row))) 'None))]))

(check-expect
 (physical->logical (PhysicalLoc 16 28) 4 (BoardSpec 'blue 12 16 4))
 (Some (LogicalLoc 0 2)))
(check-expect
 (physical->logical (PhysicalLoc 16 17) 4 (BoardSpec 'blue 12 16 4))
 (Some (LogicalLoc 0 3)))
(check-expect
 (physical->logical (PhysicalLoc 1 1) 10 (BoardSpec 'blue 20 30 6))
 'None)
(check-expect
 (physical->logical (PhysicalLoc 240 240) 10 (BoardSpec 'blue 20 30 6))
 'None)
(check-expect
 (physical->logical (PhysicalLoc 88 108) 10 (BoardSpec 'blue 20 30 6))
 (Some (LogicalLoc 3 5)))

;; Convert logical locations to strings such as "A1", "B3", etc.
;; Note the letter "I" is skipped in Go labeling.
;; When you get a column past "Z", use "AA", then "BB", then "CC", etc.
;; When you get past "ZZ", use "AAA", then "BBB", etc.
(: logical->string : LogicalLoc -> String)
(define (logical->string loc)
  (match loc
    [(LogicalLoc col row)
     (string-append
      (string-append*
       (make-list
        (+ 1 (quotient col 25))  
        (list-ref '("A" "B" "C" "D" "E" "F" "G" "H" "J" "K" "L" "M" "N" "O" "P"
                        "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
                  (remainder col 25))))
      (number->string (+ row 1)))]))

(check-expect (logical->string (LogicalLoc 0 9)) "A10")
(check-expect (logical->string (LogicalLoc 27 18)) "CC19")
(check-expect (logical->string (LogicalLoc 49 27)) "ZZ28")

;; Returns true if and only if two specified logical
;; locations are the same
(: logicalloc=? : LogicalLoc LogicalLoc -> Boolean)
(define (logicalloc=? loc1 loc2)
  (match (list loc1 loc2)
    [(list (LogicalLoc col1 row1) (LogicalLoc col2 row2))
     (if (and (= col1 col2) (= row1 row2)) #t #f)]))

(check-expect (logicalloc=? (LogicalLoc 0 2) (LogicalLoc 0 2)) #t)
(check-expect (logicalloc=? (LogicalLoc 5 20) (LogicalLoc 5 21)) #f)

;; checks whether a board is square
(: valid-board? : Board -> Boolean)
(define (valid-board? board)
  (if (= (vector-length board) 0) #f
      (local
        {(: lp : Integer -> Boolean)
         (define (lp acc)
           (if (= acc (vector-length board)) #t
               (and (= (vector-length (vector-ref board acc))
                       (vector-length board))
                    (lp (+ acc 1)))))}
        (lp 0))))

(check-expect (valid-board? (vector)) #f)
(check-expect (valid-board? (sample-board 1)) #t)
(check-expect (valid-board? (vector (vector 'None 'None)
                                    (vector 'None))) #f)

;; goof: creates a generic Go struct containing a given board
(: goof : Board -> Go)
(define (goof board)
  (Go board 'black '() 'None '() '() 0))

(check-expect (goof (empty-board 2))
              (Go (empty-board 2) 'black '() 'None '() '() 0))

;; valid-loc?: checks whether a given logical location is
;; valid in a given game
(: valid-loc? : Go LogicalLoc -> Boolean)
(define (valid-loc? go loc)
  (match (list go loc)
    [(list (Go board next history lt lto lts cp) (LogicalLoc col row))
     (local
       {(define max (- (vector-length board) 1))}
       (if (or (< col 0) (> col max) (< row 0) (> row max)) #f #t))]))

(check-expect (valid-loc? (goof (empty-board 3)) (LogicalLoc 0 3)) #f)
(check-expect (valid-loc? (goof (empty-board 3)) (LogicalLoc 2 2)) #t)

;; Return the stone at the specified location
;; on the board, or indicate it is unoccupied
(: board-ref : Go LogicalLoc -> (Optional Stone))
(define (board-ref go loc)
  (match go
    [(Go board next history lt lto lts cp)
     (match loc
       [(LogicalLoc col row)
        (cond
          [(not (valid-board? board))
           (error "board-ref: board is not square")]
          [(not (valid-loc? (goof board) loc))
           (error "board-ref: given location exceeds board dimension")]
          [else (vector-ref (vector-ref board col) row)])])]))

(check-expect
 (board-ref (Go (sample-board 1) 'white '() 'None '() '() 0)
            (LogicalLoc 0 0)) 'None)
(check-expect
 (board-ref (Go (sample-board 1) 'black '() 'None '() '() 0)
            (LogicalLoc 3 3)) (Some 'black))
(check-error
 (board-ref (Go (sample-board 1) 'white '() 'None '() '() 0) (LogicalLoc 5 2))
 "board-ref: given location exceeds board dimension")
(check-error
 (board-ref (Go (vector (vector 'None 'None) (vector 'None))
                'black '() 'None '() '() 0)
            (LogicalLoc 0 0))
 "board-ref: board is not square")

;; empty-board: creates an empty board with a specified dimension
(: empty-board : Integer -> Board)
(define (empty-board dim)
  (build-vector dim
                (lambda ([i : Integer])
                  (make-vector dim (cast 'None (Optional Stone))))))

(check-expect (empty-board 2)
              (vector (vector 'None 'None) (vector 'None 'None)))

;; board-set!: stores a specified stone at the given location
(: board-set! : Go LogicalLoc (Optional Stone) -> Void)
(define (board-set! go loc stone)
  (match (list go loc)
    [(list (Go board next history lt lto lts cp) (LogicalLoc col row))
     (vector-set! (vector-ref board col) row stone)]))

(: csample-board : (Vectorof (Vectorof (Optional Stone))))
(define csample-board (sample-board 1))
(board-set! (Go csample-board 'white '() 'None '() '() 0)
            (LogicalLoc 0 1) (Some 'black))
(check-expect
 (board-ref (Go csample-board 'white '() 'None '() '() 0) (LogicalLoc 0 1))
 (Some 'black))

(: empty2 : (Vectorof (Vectorof (Optional Stone))))
(define empty2 (empty-board 2))
(board-set! (Go empty2 'white '() 'None '() '() 0)
            (LogicalLoc 0 0) (Some 'black))
(check-expect
 (self (Go empty2 'white '() 'None '() '() 0))
 (Go (vector (vector (Some 'black) 'None) (vector 'None 'None))
     'white '() 'None '() '() 0))

;; board-copy: creates an independent copy of a board
(: board-copy : Board -> Board)
(define (board-copy board)
  (local
    {(define dim (vector-length board))
     (: self : Integer -> Integer)
     (define (self x) x)}
    (foldl
     (lambda ([x : Integer] [b : Board])
       (begin
         (vector-set!
          b x (foldl
               (lambda ([y : Integer] [v : (Vectorof (Optional Stone))])
                 (begin
                   (vector-set!
                    v y (vector-ref (vector-ref board x) y)) v))
               (vector-ref b x) (build-list dim self))) b))
     (empty-board dim) (build-list dim self))))

(: dsample-board : Board)
(define dsample-board (sample-board 1))
(define copy (board-copy dsample-board))
(check-expect (board-copy dsample-board) dsample-board)
(board-set! (Go copy 'black '() 'None '() '() 0) (LogicalLoc 0 0) (Some 'white))
(check-expect ((lambda ([b : Board]) b) dsample-board) (sample-board 1)) 

;; occupied?: checks whether a given location in a game is occupied
(: occupied? : Go LogicalLoc -> Boolean)
(define (occupied? go loc)
  (not (symbol? (board-ref go loc))))

(check-expect (occupied? (goof (sample-board 1)) (LogicalLoc 0 0)) #f)
(check-expect (occupied? (goof (sample-board 1)) (LogicalLoc 2 1)) #t)

;; adjacent-locs: outputs a list of locations in a
;; game adjacent to a given intersection
(: adjacent-locs : Go LogicalLoc -> (Listof LogicalLoc))
(define (adjacent-locs go loc)
  (if (valid-loc? go loc)
      (match (list go loc)
        [(list (Go board next history lt lto lts cp) (LogicalLoc col row))
         (append (if (valid-loc? go (LogicalLoc (+ col 1) row))
                     (list (LogicalLoc (+ col 1) row)) '())
                 (if (valid-loc? go (LogicalLoc col (+ row 1)))
                     (list (LogicalLoc col (+ row 1))) '())
                 (if (valid-loc? go (LogicalLoc (- col 1) row))
                     (list (LogicalLoc (- col 1) row)) '())
                 (if (valid-loc? go (LogicalLoc col (- row 1)))
                     (list (LogicalLoc col (- row 1))) '()))])
      (error "adjacent-locs: input location is invalid")))

(check-expect (adjacent-locs (goof (sample-board 1)) (LogicalLoc 3 2))
              (list (LogicalLoc 3 3) (LogicalLoc 2 2) (LogicalLoc 3 1)))
(check-error (adjacent-locs (goof (sample-board 1)) (LogicalLoc -1 2))
             "adjacent-locs: input location is invalid")

;; has-liberties?: checks whether a location
;; has any non-occupied adjacent locations
(: has-liberties? : Go LogicalLoc -> Boolean)
(define (has-liberties? go loc)
  (not (andmap (lambda ([x : LogicalLoc]) (occupied? go x))
               (adjacent-locs go loc))))

(check-expect (has-liberties? (goof (empty-board 2)) (LogicalLoc 0 0)) #t)
(check-expect (has-liberties? (goof (sample-board 1)) (LogicalLoc 3 2)) #f)
(check-expect (has-liberties? (goof (sample-board 1)) (LogicalLoc 0 1)) #t)

;; color: identifies the color of the stone in a specified location
;; in a game and outputs 'None if it is unoccupied
(: color : Board LogicalLoc -> (U 'None Stone))
(define (color board loc)
  (match (board-ref (goof board) loc)
    ['None 'None]
    [(Some 'black) 'black]
    [(Some 'white) 'white]))

(check-expect (color (sample-board 1) (LogicalLoc 1 0)) 'black)
(check-expect (color (sample-board 1) (LogicalLoc 3 1)) 'white)
(check-expect (color (sample-board 1) (LogicalLoc 0 0)) 'None)

;; other-player: returns the color opposite the next player
;; in a Go struct
(: other-player : Go -> Stone)
(define (other-player go)
  (match go
    [(Go board next history lt lto lts cp)
     (if (symbol=? next 'black) 'white 'black)]))

(check-expect (other-player (goof (sample-board 1))) 'white)

;; identify-chain: outputs the chain containing the specified locations,
;; if possible. The first input list is of locations to be checked and the
;; second list is of locations that we know are part of the chain
(: identify-chain : Board (Listof LogicalLoc) (Listof LogicalLoc)
   -> (Optional (Listof LogicalLoc)))
(define (identify-chain board to-explore marked)
  (match to-explore
    ['() (Some marked)]
    [(cons hd tl)
     (cond
       [(has-liberties? (goof board) hd) 'None]
       [(symbol=? 'None (color board hd)) 'None]  
       [(local
          {(: color=hd? : LogicalLoc -> Boolean)
           (define (color=hd? loc)
             (if (symbol=? (color board hd) (color board loc)) #t #f))
           (define new-to-explore
             (filter
              (lambda ([x : LogicalLoc])
                (and (color=hd? x) (boolean? (member x marked))))
              (adjacent-locs (goof board) hd)))}
          (identify-chain board (append tl new-to-explore)
                          (append marked new-to-explore)))])]))

(check-expect
 (identify-chain (vector (vector (Some 'white) (Some 'black) (Some 'black))
                         (vector 'None (Some 'white) (Some 'white))
                         (vector 'None 'None 'None))
                 (list (LogicalLoc 0 1)) (list (LogicalLoc 0 1)))
 (Some (list (LogicalLoc 0 1) (LogicalLoc 0 2))))
(check-expect
 (identify-chain (sample-board 1) (list (LogicalLoc 0 0))
                 (list (LogicalLoc 0 0)))
 'None)
(check-expect
 (identify-chain (sample-board 1) (list (LogicalLoc 0 3))
                 (list (LogicalLoc 0 3)))
 'None)

;; remove-chain: if the specified location is part of a chain,
;; remove all stones in the chain.
(: remove-chain : Go LogicalLoc -> Go)
(define (remove-chain go loc)
  (match go
    [(Go board next history lt lto lts cp)
     (local
       {(define chain
          (match (identify-chain (Go-board go) (list loc) (list loc))
            ['None 'None] [(Some locs) locs]))
        (: lp : (Listof LogicalLoc) (Listof LogicalLoc) -> (Listof LogicalLoc))
        (define (lp locs acc)
          (match locs
            ['() acc]
            [(cons hd tl)
             (begin (board-set! go hd 'None)
                    (cons hd (lp tl acc)))]))}
       (match chain
         ['None go]
         [(cons hd tl)
          (match (board-ref go loc)
            ['None go]
            [(Some stone)
             (if (symbol=? stone next)
                 (Go board next history lt lto (lp chain lts) cp)
             (Go board next history lt (lp chain lto) lts cp))])]))]))

(check-expect
 (remove-chain (Go (vector (vector (Some 'white) (Some 'black) (Some 'black))
                           (vector 'None (Some 'white) (Some 'white))
                           (vector 'None 'None 'None))
                   'black '() 'None (list (LogicalLoc 2 1)) '() 0)
               (LogicalLoc 0 1))
 (Go (vector (vector (Some 'white) 'None 'None)
             (vector 'None (Some 'white) (Some 'white))
             (vector 'None 'None 'None))
     'black '() 'None (list (LogicalLoc 2 1))
     (list (LogicalLoc 0 1) (LogicalLoc 0 2)) 0))

(check-expect
 (remove-chain (goof (sample-board 1)) (LogicalLoc 1 1))
 (goof (sample-board 1)))

;; adjacent-opposite: lists adjacent locations with stones of the opposite color
(: adjacent-opposite : Go LogicalLoc -> (Listof LogicalLoc))
(define (adjacent-opposite go loc)
  (match go
    [(Go board next history lt lto lts cp)
     (match (board-ref go loc)
       ['None '()]
       [(Some 'black)
        (filter
         (lambda ([x : LogicalLoc])
           (symbol=? (color board x) 'white)) (adjacent-locs go loc))]
       [(Some 'white)
        (filter
         (lambda ([x : LogicalLoc])
           (symbol=? (color board x) 'black)) (adjacent-locs go loc))])]))

(check-expect (adjacent-opposite (goof (sample-board 1)) (LogicalLoc 2 1))
              (list (LogicalLoc 1 1)))
(check-expect (adjacent-opposite (goof (sample-board 1)) (LogicalLoc 1 0)) '())

;; capture: removes adjacent chains of the opposite color if
;; the specified location is occupied, and does
;; nothing if the specified location is empty
(: capture : Go LogicalLoc -> Go)
(define (capture go loc)
  (local
    {(: lp : (Listof LogicalLoc) -> Go)
     (define (lp locs)
       (match locs
         ['() go]
         [(cons hd tl)
          (remove-chain (lp tl) hd)]))}
    (lp (adjacent-opposite go loc))))

(check-expect
 (capture
  (Go (vector (vector 'None (Some 'black) (Some 'white))
                (vector (Some 'black) (Some 'white) (Some 'white))
                (vector 'None (Some 'black) (Some 'black)))
      'black '() 'None '() '() 0) (LogicalLoc 2 1))
 (Go (vector (vector 'None (Some 'black) 'None)
         (vector (Some 'black) 'None 'None)
         (vector 'None (Some 'black) (Some 'black)))
     'black '() 'None (list (LogicalLoc 1 1) (LogicalLoc 1 2)
                                (LogicalLoc 0 2)) '() 0))
              
(check-expect
 (capture (goof (vector (vector 'None (Some 'white))
                        (vector (Some 'black) 'None))) (LogicalLoc 0 1))
 (goof (vector (vector 'None (Some 'white)) (vector (Some 'black) 'None))))
         
;; apply-move: places a stone and performs all associated game mechanics
(: apply-move : Go LogicalLoc -> Go)
(define (apply-move go loc)
  (match go
    [(Go board next history lt lto lts cp)
     (local
       {(define nhistory (append (list (board-copy board)) history))
        (define cleango (Go board next history 'None '() '() cp))}
       (begin
         (board-set! go loc (Some next))
         (match (remove-chain (capture cleango loc) loc)
           [(Go nboard next history 'None nlto nlts ncp)
            (Go nboard (other-player go) nhistory
                (Some loc) nlto nlts 0)])))]))

(check-expect
 (apply-move (Go (sample-board 1) 'black '() 'None '() '() 1) (LogicalLoc 1 3))
 (Go (vector
      (vector 'None 'None (Some 'black) 'None)
      (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
      (vector 'None (Some 'white) (Some 'white) 'None)
      (vector 'None (Some 'white) 'None (Some 'black)))
     'white (list (sample-board 1)) (Some (LogicalLoc 1 3))
     (list (LogicalLoc 0 3)) '() 0))
(check-expect
 (apply-move (Go (vector (vector (Some 'black) 'None)
                         (vector 'None (Some 'black)))
                 'white '() 'None (list (LogicalLoc 0 0)) '() 0)
             (LogicalLoc 0 1))
 (Go (vector (vector (Some 'black) 'None)
             (vector 'None (Some 'black)))
     'black (list (vector (vector (Some 'black) 'None)
                          (vector 'None (Some 'black))))
     (Some (LogicalLoc 0 1)) '() (list (LogicalLoc 0 1)) 0))
(check-expect
 (apply-move (Go (vector (vector (Some 'black) 'None)
                         (vector (Some 'white) (Some 'black)))
                 'white '() 'None '() '() 0) (LogicalLoc 0 1))
 (Go (vector (vector 'None (Some 'white))
             (vector (Some 'white) 'None))
     'black (list (vector (vector (Some 'black) 'None)
                         (vector (Some 'white) (Some 'black))))
     (Some (LogicalLoc 0 1)) (list (LogicalLoc 1 1) (LogicalLoc 0 0)) '() 0))

;; repeat?: checks if a move in a game matches a previous state of the board
(: repeat? : Go LogicalLoc -> Boolean)
(define (repeat? go loc)
  (match go
    [(Go board next history lt lto lts cp)
     (if (boolean?
          (member (Go-board (apply-move
                             (Go (board-copy board) next history lt lto lts cp)
                                        loc))
                  history))
         #f #t)]))

(check-expect
 (repeat? (Go (sample-board 1) 'black
              (list (vector
                     (vector 'None 'None (Some 'black) 'None)
                     (vector (Some 'black) (Some 'black)
                             (Some 'black) (Some 'black))
                     (vector 'None (Some 'white) (Some 'white) 'None)
                     (vector 'None (Some 'white) 'None (Some 'black)))
                    (empty-board 3))  'None '() '() 0) (LogicalLoc 1 3)) #t)
(check-expect
 (repeat? (Go (sample-board 1) 'black (list (empty-board 3)) 'None '() '() 0)
          (LogicalLoc 0 0)) #f)

;; two-passes?: determines whether the last two moves have been passes
(: two-passes? : Go -> Boolean)
(define (two-passes? go)
  (if (= (Go-consecutive-passes go) 2) #t #f))

(check-expect (two-passes? (goof (sample-board 1))) #f)
(check-expect (two-passes? (Go (sample-board 1) 'white '() 'None '() '() 2)) #t)

;; Given the current state of the game, if the player whose turn it currently
;; is places a stone at the specified location, is this a legal move? 
(: legal-move? : Go LogicalLoc -> Boolean)
(define (legal-move? go loc)
  (not (or (repeat? go loc) (occupied? go loc) (two-passes? go))))

(check-expect
 (legal-move? (Go (sample-board 1) 'black
                  (list (vector
                         (vector 'None 'None (Some 'black) 'None)
                         (vector (Some 'black) (Some 'black)
                                 (Some 'black) (Some 'black))
                         (vector 'None (Some 'white) (Some 'white) 'None)
                         (vector 'None (Some 'white) 'None (Some 'black)))
                        (empty-board 3)) 'None '() '() 0) (LogicalLoc 1 3)) #f)
(check-expect (legal-move? (Go (sample-board 1) 'white '() 'None '() '() 0)
                           (LogicalLoc 0 3)) #f)
(check-expect (legal-move? (Go (sample-board 1) 'white '() 'None '() '() 0)
                           (LogicalLoc 0 1)) #t)

;; determines whether a given location is in a color's area
(: in-area? : Board (Listof LogicalLoc)
   (Listof LogicalLoc) Stone -> Boolean)
(define (in-area? board to-explore marked player)
  (match to-explore
    ['() #t]
    [(cons hd tl)
     (local
       {(: adjacent-enemy : LogicalLoc -> (Listof LogicalLoc))
        (define (adjacent-enemy loc)
          (filter
           (lambda ([x : LogicalLoc])
             (not (or (symbol=? (color board x) 'None)
                      (symbol=? (color board x) player))))
           (adjacent-locs (goof board) loc)))}
       (cond
         [(symbol=? (color board hd) player) #t] 
         [(not (empty? (adjacent-enemy hd))) #f]
         [(not (symbol=? 'None (color board hd))) #f]  
         [else
          (local
            {(define new-to-explore
               (filter
                (lambda ([x : LogicalLoc])
                  (and (symbol=? (color board x) 'None)
                       (boolean? (member x marked))))
                (adjacent-locs (goof board) hd)))}
            (in-area? board (append tl new-to-explore)
                            (append marked new-to-explore) player))]))]))

(check-expect (in-area? (sample-board 1) (list (LogicalLoc 0 0))
                        (list (LogicalLoc 0 0)) 'black) #t)
(check-expect (in-area? (sample-board 1) (list (LogicalLoc 1 0))
                        (list (LogicalLoc 1 0)) 'black) #t)
(check-expect (in-area? (sample-board 1) (list (LogicalLoc 3 2))
                        (list (LogicalLoc 3 2)) 'white) #f)
(check-expect (in-area? (sample-board 1) (list (LogicalLoc 2 1))
                        (list (LogicalLoc 2 1)) 'black) #f)

;; area-size: returns the number of locations in a given player's area in a game
(: area-size : Go Stone -> Integer)
(define (area-size go player)
  (match go
    [(Go board next history lt lto lts cp)
     (length
      (foldl
       (lambda ([y : Integer] [b1 : (Listof LogicalLoc)])
         (foldl
          (lambda ([x : Integer] [b2 : (Listof LogicalLoc)])
            (if (in-area? board (list (LogicalLoc x y))
                          (list (LogicalLoc x y)) player)
                (cons (LogicalLoc x y) b2) b2))
          b1 (build-list (vector-length board) (lambda ([x : Integer]) x))))
       '() (build-list (vector-length board) (lambda ([x : Integer]) x))))]))

(check-expect (area-size (goof (sample-board 1)) 'black) 7)
(check-expect (area-size (goof (sample-board 1)) 'white) 4)
(check-expect (area-size (goof (vector (vector (Some 'white) 'None)
                                       (vector 'None (Some 'white))))
                         'white) 4)
(check-expect (area-size (goof (vector (vector (Some 'white) 'None)
                                       (vector 'None (Some 'white))))
                         'black) 0)

;; outcome: reports the outcome of a completed game
(: outcome : Go -> Outcome)
(define (outcome go)
  (local
    {(define black (area-size go 'black))
     (define white (area-size go 'white))}
    (Outcome black white (cond [(> black white) 'black]
                               [(< black white) 'white]
                               [else 'draw]))))

(check-expect (outcome (goof (sample-board 1)))
              (Outcome 7 4 'black))

;; outcome->string: returns a string describing a given outcome
(define (outcome->string o)
  (match o
    [(Outcome black white winner)
     (match winner
       ['black (string-append "Black won with " (number->string black)
                              " points to white's " (number->string white))]
       ['white (string-append "White won with " (number->string white)
                              " points to black's " (number->string black))]
       ['draw (string-append "Black and white tied at "
                             (number->string black) " points")])]))

(check-expect (outcome->string (outcome (goof (sample-board 1))))
              "Black won with 7 points to white's 4")
     
;; If the named location is unoccupied,
;; put the stone there and advance the player, and
;; return Some Go struct. Return 'None if the location is already occupied.
;; Raise an error if the stone to be placed does not match the color of the
;;  player whose turn is indicated by the Go struct.
(: put-stone-at : LogicalLoc Stone Go -> (Optional Go))
(define (put-stone-at loc stone go)
  (match (list loc go)
    [(list (LogicalLoc col row) (Go board next history lt lto lts cp))
     (cond
       [(not (symbol=? next stone))
        (error "put-stone-at: stone and player color do not match")]
       [(symbol? (board-ref go loc))
        (local
          {(define newgo
             (Go board (other-player go)
                 (append (list (board-copy board)) history) lt lto lts cp))}
          (begin
            (board-set! go loc (Some stone))
            (Some newgo)))]
       [else 'None])]
    [_ 'None]))

(define bsample-board (sample-board 1))
(check-expect
 (put-stone-at (LogicalLoc 0 1) 'white
               (Go bsample-board 'white '() 'None '() '() 0))
 (Some
  (Go (vector
       (vector 'None (Some 'white) (Some 'black) (Some 'white))
       (vector (Some 'black) (Some 'black) (Some 'black) 'None)
       (vector 'None (Some 'white) (Some 'white) 'None)
       (vector 'None (Some 'white) 'None (Some 'black)))
      'black (list (sample-board 1))  'None '() '() 0)))
(check-error
 (put-stone-at (LogicalLoc 0 1) 'black
               (Go bsample-board 'white '() 'None '() '() 0))
 "put-stone-at: stone and player color do not match")
(check-expect
 (put-stone-at (LogicalLoc 2 2) 'white
               (Go bsample-board 'white '() 'None '() '() 0)) 'None)

(define-struct OldGo
  ([dimension : Integer]
   [black-stones : (Listof LogicalLoc)]
   [white-stones : (Listof LogicalLoc)]
   [next-to-play : Stone]))

;; convert-go: converts a Go struct from list to vector format
;; (with empty history)
(: convert-go : OldGo -> Go)
(define (convert-go go)
  (local
    {(define dim (OldGo-dimension go))
     (: insertb : LogicalLoc Go -> Go)
     (define (insertb loc go)
       (begin (board-set! go loc (Some 'black)) go))
     (: insertw : LogicalLoc Go -> Go)
     (define (insertw loc go)
       (begin (board-set! go loc (Some 'white)) go))}
    (match go
      [(OldGo dim bs ws next)
       (foldl insertb (foldl insertw (Go (empty-board dim) next '()
                                         'None '() '() 0) ws) bs)]))) 

(check-expect
 (convert-go
  (OldGo 4 (list (LogicalLoc 0 2) (LogicalLoc 1 0) (LogicalLoc 1 1)
                 (LogicalLoc 1 2) (LogicalLoc 3 3))
         (list (LogicalLoc 0 3) (LogicalLoc 2 1)
               (LogicalLoc 2 2) (LogicalLoc 3 1))
         'white))
 (Go (sample-board 1) 'white '() 'None '() '() 0))

;; checks whether the cell size, margin, and stone radius
;; are all positive; the stone radius is less than half the
;; cell size; and the margin exceeds the stone radius.
(: valid-board-spec? : BoardSpec -> Boolean)
(define (valid-board-spec? board)
  (match board
    [(BoardSpec bcolor csize msize srad)
     (if
      (and
       (< 0 csize) (< 0 msize) (< 0 srad) (< srad (/ csize 2)) (< srad msize))
      #t #f)]))

(check-expect (valid-board-spec? (BoardSpec 'brown 12 16 4)) #t)
(check-expect (valid-board-spec? (BoardSpec 'brown -2 10 3)) #f)
(check-expect (valid-board-spec? (BoardSpec 'blue 12 16 -1)) #f)
(check-expect (valid-board-spec? (BoardSpec 'brown 25 10 11)) #f)
(check-expect (valid-board-spec? (BoardSpec 'black 10 16 9)) #f)

;; column->string: outputs a string of letters corresponding to
;; a given column
(: column->string : Integer -> String)
(define (column->string col)
  (string-append*
   (make-list
    (+ 1 (quotient col 25))  
    (list-ref
     '("A" "B" "C" "D" "E" "F" "G" "H" "J" "K" "L" "M" "N" "O" "P"
           "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
     (remainder col 25))))) 

(check-expect (column->string 0) "A")
(check-expect (column->string 27) "CC")
(check-expect (column->string 49) "ZZ")

;; format-time: turns a number of tenths of seconds into a string
(: format-time : Integer -> String)
(define (format-time t)
  (cond
    [(< t 0) (error "format-time: values should not be negative")]
    [(string-append (number->string (quotient t 600))
                 "."
                 (string-append
                  (if (> 10 (quotient (remainder t 600) 10))
                      "0" "")
                  (number->string (quotient (remainder t 600) 10)))
                 "."
                 (number->string (remainder (remainder t 60) 10)))]))

(check-expect (format-time 673) "1.07.3")
(check-expect (format-time 349) "0.34.9")

;; displays a game board according to specifications given in a World struct
(: draw-board : World -> Image)
(define (draw-board world)
  (match world
    [(World (BoardSpec bcolor csize msize srad)
            (Go board next history lt lto lts cp)
            status-message bt wt hover)
     (local
       {(define dim (vector-length board))
        (define timers
          (beside (overlay (above (text (format-time bt) 15 'white)
                                  (text "Black" 12 'white))
                           (rectangle
                            (max (+ (image-width (text (format-time bt)
                                                       12 'white)) 15)
                                 (/ (+ (* (- dim 1) csize) (* 2 msize)) 3))
                            40 'solid 'black))
                  (overlay (above (text (format-time (+ bt wt)) 15 'black)
                                  (text "Total" 12 'black))
                           (rectangle
                            (max (+ (image-width (text (format-time (+ bt wt))
                                                       12 'white)) 15)
                                 (/ (+ (* (- dim 1) csize) (* 2 msize)) 3))
                            40 'solid bcolor))
                  (overlay (above (text (format-time wt) 15 'black)
                                  (text "White" 12 'black))
                           (rectangle
                            (max (+ (image-width (text (format-time wt)
                                                       12 'black)) 15)
                                 (/ (+ (* (- dim 1) csize) (* 2 msize)) 3))
                            40 'outline 'black))))
        (define grid
          (foldl
           (lambda ([y : Integer] [b1 : Image])
             (foldl
              (lambda ([x : Integer] [b2 : Image])
                (match (logical->physical
                        (LogicalLoc x y) dim
                        (BoardSpec bcolor csize msize srad))
                  [(PhysicalLoc px py)
                   (place-image
                    (overlay
                     (if (not (boolean? (member (Some (LogicalLoc x y))
                                                (list lt))))
                         (square (* srad 2.5) 'outline 'red) empty-image)
                     (cond [(not (boolean? (member (LogicalLoc x y) lto)))
                            (square (/ csize 3) 'solid next)]
                           [(not (boolean? (member (LogicalLoc x y) lts)))
                            (square (/ csize 3) 'solid
                                      (other-player (World-game world)))]
                           [else empty-image])
                     (match (vector-ref (vector-ref board x) y)
                       ['None empty-image]
                       [(Some 'white) (circle srad 'solid 'white)]
                       [(Some 'black) (circle srad 'solid 'black)]))
                                px py b2)]))
              b1 (build-list dim (lambda ([x : Integer]) x))))
           (overlay
            (foldr
             above empty-image
             (build-list
              (- dim 1)
              (lambda ([x : Integer])
                (foldr
                 beside empty-image
                 (build-list
                  (- dim 1)
                  (lambda [(x : Integer)] (square csize 'outline 'black)))))))
            (square (+ (* 2 msize) (* (- dim 1) csize)) 'solid bcolor))
           (build-list dim (lambda ([x : Integer]) x))))
        (define status
          (overlay
           (text status-message 15 'black)
           (rectangle
            (max (+ (image-width (text status-message 15 'black)) 20)
                 (+ (* 2 msize) (* (- dim 1) csize))
                 (image-width timers)) 40 'solid bcolor)))
        (define rows
          (foldl
           above
           empty-image
           (map
            (lambda ([x : String])
              (overlay (text x 12 'black) (square csize 'solid 'white)))
            (build-list
             dim  
             (lambda ([x : Integer]) (number->string (+ x 1)))))))
        (define cols
          (beside
           (rectangle (- msize (/ csize 2)) csize 'solid 'white)
           (foldr
            beside
            empty-image
            (map
             (lambda ([x : String])
               (overlay (text x 12 'black) (square csize 'solid 'white)))
             (build-list
              dim 
              (lambda ([x : Integer]) (column->string x)))))))}
       (above/align
        "left"
        (beside/align
         "center"
         (match hover
           ['None grid]
           [(Some h)
            (match (logical->physical h dim (World-spec world))
              [(PhysicalLoc hx hy)
               (place-image (circle srad 128 next) hx hy grid)])])
         rows)       
        cols
        (scale (/ (image-width grid) (image-width status))
               (above status timers))))]))

(: sample-world : Integer -> World)
(define (sample-world i)
  (World (BoardSpec 'moccasin 20 15 6)
         (convert-go
          (OldGo 10 (list (LogicalLoc 0 2) (LogicalLoc 9 9))
                 (list (LogicalLoc 3 5) (LogicalLoc 9 8)) 'black))
         "Welcome to Go!" 0 0 (Some (LogicalLoc 0 0))))

(draw-board (World (BoardSpec 'moccasin 30 20 8)
                   (Go (sample-board 1) 'white '() (Some (LogicalLoc 2 0))
                       (list (LogicalLoc 0 0) (LogicalLoc 0 1))
                       (list (LogicalLoc 2 0)) 0)
                   "Welcome to Go!" 300 230 (Some (LogicalLoc 0 0))))
(draw-board
 (World (BoardSpec 'cyan 15 12 5)
        (convert-go
         (OldGo 27 (list (LogicalLoc 26 20) (LogicalLoc 9 9))
                (list (LogicalLoc 3 5) (LogicalLoc 9 8)) 'white))
        "black moved to BB20"  90 117 (Some (LogicalLoc 13 7))))

;; react-to-mouse: places a stone if the player clicks within a
;; stone's radius of an unoccupied intersection and shows a
;; transluscent stone if the player can place one
(: react-to-click : World Integer Integer Mouse-Event -> World)
(define (react-to-click world x y mouse)
  (match world
    [(World spec go status bt wt hover)
     (local
       {(define  phys-cursor-loc          
          (PhysicalLoc x y))
        (define dim (vector-length (Go-board go)))
        (define log-cursor-loc
          (physical->logical phys-cursor-loc dim spec))}
       (match log-cursor-loc
         [(Some loc)
          (match mouse
            ["button-down"
             (cond
               [(legal-move? go loc)
                (World spec (apply-move go loc)
                       (string-append (symbol->string (Go-next-to-play go))
                                      " moved to " (logical->string loc))
                       bt wt hover)]
               [else
                (World spec go
                       (string-append (symbol->string (Go-next-to-play go))
                                      " attempted an illegal move")
                       bt wt hover)])]
            ["move" (World spec go status bt wt (Some loc))]
            [_ world])]
         [_ world]))]))

(check-expect
 (react-to-click (sample-world 1) 36 56 "button-down")
 (World (BoardSpec 'moccasin 20 15 6)
        (apply-move (World-game (sample-world 1)) (LogicalLoc 1 7))
        "black moved to B8" 0 0 (Some (LogicalLoc 0 0))))
(check-expect
 (react-to-click (sample-world 1) 36 56 "button-down")
 (World (BoardSpec 'moccasin 20 15 6)
        (apply-move (World-game (sample-world 1)) (LogicalLoc 1 7))
        "black moved to B8" 0 0 (Some (LogicalLoc 0 0))))
(check-expect (react-to-click (sample-world 1) 0 60 "button-down")
              (sample-world 1))
(check-expect (react-to-click (sample-world 1) 190 190 "button-down")
              (sample-world 1))
(check-expect (react-to-click (sample-world 1) 67 43 "button-up")
              (sample-world 1))
(check-expect (react-to-click (sample-world 1) 15 155 "button-down")
              (World (BoardSpec 'moccasin 20 15 6)
                     (World-game (sample-world 1))
                     "black attempted an illegal move"  0 0
                     (Some (LogicalLoc 0 0))))

;; stone->string: converts an optional
;; stone to string format for saving a game
(: stone->string : (Optional Stone) -> String)
(define (stone->string stone)
  (match stone ['None "_"] [(Some 'black) "*"] [(Some 'white) "o"]))

(check-expect (stone->string (Some 'black)) "*")

;; board->string: converts a board into string format
(: board->string : Board -> String)
(define (board->string board)
  (local
    {(define dim (vector-length board))}
    (foldl
     (lambda ([x : Integer] [acc1 : String])
       (string-append
        acc1
        (foldl (lambda ([y : Integer] [acc2 : String])
                 (string-append
                  acc2 (stone->string (vector-ref (vector-ref board x) y))))
         "" (build-list dim (lambda [(x : Integer)] x)))
        (if (= x (- dim 1)) "" "|")))
        "" (build-list dim (lambda [(x : Integer)] x))))) 
         
(check-expect (board->string (sample-board 1)) "__*o|***_|_oo_|_o_*")
(check-expect (board->string (empty-board 2)) "__|__")

;; go->string: converts a Go struct into string format
(: go->string : Go -> String)
(define (go->string go)
  (match go
    [(Go board next history lt lto lts cp)
     (string-append
      (if (symbol=? next 'black) "*" "o")
      "~" (board->string board) "~"
      (foldr (lambda ([x : Integer] [acc : String])
               (string-append (board->string (list-ref history x))
                              (if (= x (- (length history) 1)) "" "!")
                              acc))
             "" (build-list (length history) (lambda ([x : Integer]) x)))
      "~" (number->string cp))]))
      
(check-expect (go->string (goof (empty-board 2))) "*~__|__~~0")
(check-expect
 (go->string (Go (empty-board 2) 'white
                 (list (empty-board 2) (empty-board 2)) 'None '() '() 1))
 "o~__|__~__|__!__|__~1")
              

;; world->string: converts a world into string format
(: world->string : World -> String)
(define (world->string world)
  (match world
    [(World bspec go _ bt wt _)
     (string-append (number->string bt) "@" (number->string wt)
                    "@" (go->string go))]))

(check-expect
 (world->string (World (BoardSpec 'blue 20 20 8)
                       (goof (empty-board 2)) "a" 20 30 'None))
 "20@30@*~__|__~~0")

;; convert a text representation of an Integer to an Integer
;; raise an error if the string is not a number
;; return the integer part of the resulting number only
;; (this is intended only to be used with integers)
(: string->integer : String -> Integer)
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) 
        (exact-round (real-part conv))
        (error "string->integer: invalid integer"))))

;; char->stone: converts a character encoding a stone into an optional stone
(: char->stone : Char -> (Optional Stone))
(define (char->stone s)
  (match s
    [#\* (Some 'black)]
    [#\o (Some 'white)]
    [_ 'None]))

(check-expect (char->stone #\*) (Some 'black))

;; string->stone: converts a string encoding a stone into an optional stone
(: string->stone : String -> (Optional Stone))
(define (string->stone s)
  (match s
    ["*" (Some 'black)]
    ["o" (Some 'white)]
    ["_" 'None]
    [_ (error "string->stone: the string is not correctly formatted")]))

(check-expect (string->stone "*") (Some 'black))
(check-error (string->stone "a")
             "string->stone: the string is not correctly formatted")

;; string->board: converts a string encoding a board into a board
(: string->board : String -> Board)
(define (string->board s)
  (if (or (char=? (string-ref s 0) #\|)
          (char=? (string-ref s (- (string-length s) 1)) #\|))
      (error "string->board: the string is not correctly formatted")
      (local
        {(define cols (string-split s "|"))
         (: string->column : String -> (Vectorof (Optional Stone)))
         (define (string->column x)
           (build-vector
            (string-length x)
            (lambda ([i : Integer]) (char->stone (string-ref x i)))))}
        (build-vector (length cols) (lambda ([x : Integer])
                                      (string->column (list-ref cols x)))))))

(check-expect (string->board "__*o|***_|_oo_|_o_*") (sample-board 1))
(check-expect (string->board "__|__") (empty-board 2))
(check-error (string->board "__|__|")
             "string->board: the string is not correctly formatted")

;; string->go: converts a string encoding a Go struct into a Go
(: string->go : String -> Go)
(define (string->go s)
  (if
   (or (char=? (string-ref s 0) #\~)
       (char=? (string-ref s (- (string-length s) 1)) #\~))
   (error "string->go: the string is not correctly formatted")
   (match (string-split s "~")
     [(list next board history cp)
      (Go (string->board board)
          (match (string->stone next)
            [(Some 'black) 'black] [(Some 'white) 'white])
          (map string->board (string-split history "!"))
          'None '() '() (string->integer cp))])))
           
(check-expect (string->go "*~__|__~~0") (goof (empty-board 2)))
(check-expect
 (string->go "o~__|__~__|__!__|__~1")
 (Go (empty-board 2) 'white
                 (list (empty-board 2) (empty-board 2)) 'None '() '() 1))
(check-error (string->go "~*~__|__~~0")
             "string->go: the string is not correctly formatted")

;; string->world: converts a string encoding a World struct into a World
(: string->world : BoardSpec String -> World)
(define (string->world spec s)
  (if (or (char=? (string-ref s 0) #\@)
          (char=? (string-ref s (- (string-length s) 1)) #\@))
      (error "string->world: string is not correctly formatted")
      (match (string-split s "@")
        [(list bt wt go)
         (World spec (string->go go) "Welcome back to Go!"
                (string->integer bt) (string->integer wt) 'None)])))

(check-expect
 (string->world (BoardSpec 'moccasin 20 20 8) "20@30@*~__|__~~0")
 (World (BoardSpec 'moccasin 20 20 8)
        (goof (empty-board 2)) "Welcome back to Go!" 20 30 'None))
(check-error
 (string->world (BoardSpec 'moccasin 20 20 8) "20@30@*~__|__~~0@")
 "string->world: string is not correctly formatted")

;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
(: save-game! : World -> Void)
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
        (local
          {(define op : Output-Port (open-output-file path))}
          (begin (write-string (world->string w) op)
                 (close-output-port op)))
        (void))))

;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided BoardSpec to make a new World
;; raise an error if the user cancels or if something goes wrong
(: load-game : BoardSpec -> World)
(define (load-game bs)
  (local
    {(define path : (U Path False) (get-file))}
    (if (path? path)
        (local
          {(define ip : Input-Port (open-input-file path))
           (define w : World
             (string->world bs (port->string (open-input-file path))))}
          (begin (close-input-port ip) w))
        (error "load-game: user cancelled"))))

;; react-to-keyboard: passes to the next player if p is pressed
(: react-to-keyboard : World String -> World)
(define (react-to-keyboard world key)
  (match world
    [(World spec (Go board next history lt lto lts cp)
                  status bt wt hover)
     (match key
       ["p"
        (cond
          [(= cp 2) world]
          [(= cp 1)
           (World spec (Go board next history 'None '() '() (+ cp 1))
                  (outcome->string (outcome (World-game world)))
                  bt wt hover)]   
          [else
           (match next
             ['black
              (World spec (Go board 'white history 'None '() '() (+ cp 1))
                     "black passed" bt wt hover)]
             ['white
              (World spec (Go board 'black history 'None '() '() (+ cp 1))
                     "white passed" bt wt hover)])])]
       ["s" (begin (save-game! world) world)]
       ["l"
        (match (load-game spec)
          [(World spec2 (Go board2 _ _ _ _ _ _) _ _ _ _)
           (cond
             [(not (valid-board? board2))
              (error "react-to-keyboard: board to load is not square")]
             [(not (= (vector-length board2) (vector-length board)))
              (error "react-to-keyboard: board to load has different
                      dimension than current board")]
             [else (load-game spec)])])] 
       [_ world])]))

(check-expect
 (react-to-keyboard (sample-world 1) "p")
 (World
  (BoardSpec 'moccasin 20 15 6)
  (Go (vector (vector 'None 'None (Some 'black) 'None 'None
                   'None 'None 'None 'None 'None)
           '#(None None None None None None None None None None)
           '#(None None None None None None None None None None)
           (vector 'None 'None 'None 'None 'None (Some 'white)
                   'None 'None 'None 'None)
           '#(None None None None None None None None None None)
           '#(None None None None None None None None None None)
           '#(None None None None None None None None None None)
           '#(None None None None None None None None None None)
           '#(None None None None None None None None None None)
           (vector 'None 'None 'None 'None 'None 'None 'None 'None (Some 'white)
                   (Some 'black))) 'white '() 'None '() '() 1)
  "black passed"  0 0 (Some (LogicalLoc 0 0))))
(check-expect
 (react-to-keyboard (sample-world 1) "f")
 (sample-world 1))
(check-expect
 (react-to-keyboard
  (World (BoardSpec 'moccasin 20 20 8)
         (Go (sample-board 1) 'white '() 'None '() '() 2)
         "black moved to A1" 0 0 (Some (LogicalLoc 0 0))) "p")
 (World (BoardSpec 'moccasin 20 20 8)
        (Go (sample-board 1) 'white '() 'None '() '() 2)
        "black moved to A1" 0 0 (Some (LogicalLoc 0 0))))
(check-expect
 (react-to-keyboard
  (World (BoardSpec 'moccasin 20 20 8)
         (Go (sample-board 1) 'white '() 'None '() '() 1)
         "black moved to A1" 0 0 (Some (LogicalLoc 0 0))) "p")
 (World (BoardSpec 'moccasin 20 20 8)
        (Go (sample-board 1) 'white '() 'None '() '() 2)
        "Black won with 7 points to white's 4" 0 0 (Some (LogicalLoc 0 0))))

;; react-to-tick: increases the total and current player's
;; elapsed time if the game is not yet over
(: react-to-tick : World -> World)
(define (react-to-tick world)
  (if (two-passes? (World-game world)) world
      (match world
        [(World spec go status bt wt hover)
         (World spec go status
                (+ bt (if (symbol=? (Go-next-to-play go) 'black) 1 0))
                (+ wt (if (symbol=? (Go-next-to-play go) 'white) 1 0))
                hover)])))

(check-expect
 (react-to-tick (sample-world 1))
 (World (BoardSpec 'moccasin 20 15 6)
         (convert-go
          (OldGo 10 (list (LogicalLoc 0 2) (LogicalLoc 9 9))
                 (list (LogicalLoc 3 5) (LogicalLoc 9 8)) 'black))
         "Welcome to Go!" 1 0 (Some (LogicalLoc 0 0))))             

;; play: starts a game of Go with given dimension and board specifications
(: play : Integer BoardSpec -> World)
(define (play dim spec)
  (cond
    [(< dim 2) (error "play: dimension should be at least two")]
    [(not (valid-board-spec? spec))
     (error "play: board specifications are not valid")]
    [else
     (big-bang (World spec (Go (empty-board dim) 'black '() 'None '() '() 0)
                      "Welcome to Go!" 0 0 (Some (LogicalLoc 0 0)))  : World
       [to-draw draw-board]
       [on-mouse react-to-click]
       [on-key react-to-keyboard]
       [on-tick react-to-tick 1/10])]))

(check-error
 (play 1 (BoardSpec 'brown 12 10 4))
 "play: dimension should be at least two")
(check-error
 (play 10 (BoardSpec 'brown 12 10 6))
 "play: board specifications are not valid")

(play 9 (BoardSpec 'moccasin 20 18 8))

(test)