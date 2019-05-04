#lang racket
(require (prefix-in g: "groups3.rkt")
         "lc.rkt" lang/posn)
(provide montecarlo make-rootnode)
(define (and-sum l)
  (cond [(null? l) #t]
        [else (let ([e (car l)])
                (if e (and-sum (cdr l)) #f))]))
(define (get-neighbours nx ny)
  (let ([e1 (if (> nx 0) (cons (- nx 1) ny) '())]
        [e2 (if (< nx 8) (cons (+ nx 1) ny) '())]
        [e3 (if (> ny 0) (cons nx (- ny 1)) '())]
        [e4 (if (< ny 8) (cons nx (+ ny 1)) '())])
    (remove-all-null (list e1 e2 e3 e4))))
(define (remove-all-null l)
  (foldl
   (lambda (x y)
     (cond [(null? x) y]
           [else (cons x y)]))
   '()  l))
(define (counts a l)
  (define (helper a l i)
    (if (null? l) i
    (if (equal? a (car l)) (helper a (cdr l) (+ i 1)) (helper a (cdr l) i))))
  (helper a l 0))
(define (2dvect->list vect)
  (let ([l (build-list (vector-length vect) values)])
  (lc (vector-2d-ref vect x y)
      : x <- l
        y <- l)))
(define (2d-vector-copy vect)
        (build-vector 9
     (lambda (x) (let ([e (vector-ref vect x)])
                   (build-vector 9 (lambda (y) (vector-ref e y)))))))
(define (board-vect->board-list board-vect)
  (map car (2dvect->list board-vect) ))
(define (alter move)
  (if (equal? move 'black)
      'white
      'black))
(define (make-2d-vector r c val)
  (build-vector r (lambda (x) (make-vector c val))))
(define (vector-2d-ref vect r c)
  (vector-ref (vector-ref vect r) c))
(define (vector-2d-set! vect r c val)
  (let ([e (vector-ref vect r)])
    (vector-set! e c val)))
;v is score of that node , n is visit of that node , N is total visits
(define (UCB v N n)
  (if (= n 0) 'infi
  (+ v (* 2 (sqrt (/ (log N) n))))))

(define depth 16)
(define branch-factor 10)
(define frequency 100)
(struct rootnode (No Board posn move score visits mydepth childVec) #:transparent #:mutable)
(define (is-leaf? anode)
  (or  (null? rootnode-childVec)
       (< (rootnode-visits anode) 2)))
;(strict leaf (Node score visits) #:transparent)
;(define (montecarlo S) ; S is given board-vect
;  (define child-vec (get-child-nodes S branch-factor))
;  (define main-node (rootnode S 0 0 child-vec))
;  (define Si (get-selected child-vec))
;  (if (is-leaf? Si)
;      (exp&back-propogate Si)
;      (montecarlo Si)))
(define (set-childvec S)
  (if (null? (rootnode-childVec S))
      (set-rootnode-childVec! S (get-childVec S))
      void))
(define (get-move i myboard mymove)
    (define movex (random 9))
    (define movey (random 9))
    (if (= i 0)
         (cons myboard 'pass)
        (begin
    (let ([e (g:legal movex movey mymove (2d-vector-copy myboard))])
    (if (car e)
        (cons (cdr e) (cons movex movey))
        (get-move (- i 1) myboard mymove))))))
(define (get-childVec S)
  (define move (rootnode-move S))
  (define mydepth (rootnode-mydepth S))
  (define board (rootnode-Board S))
(define (getlist l n)
  (let* ([e (get-move 81 board move)]
         [myboard (if (equal? e 'pass) board (car e))]
         [move-posn (if (equal? e 'pass) 'pass (cdr e))]
         [l1 (append l (list (rootnode n myboard move-posn
                                       (alter move) 0 0 (+ 1 mydepth) '())))])
    (if (or (equal? e 'pass)
            (= n (- branch-factor 1)))
        l1
        (getlist l1 (+ n 1)))))
   (list->vector (getlist '() 0)))

(define (get-selected childVec N)
  (define childList (vector->list childVec))
  (define max 0)
  (define maxChild (car childList))
    (define (helper l)
      (cond [(not (null? l))
      (let* ([a (car l)]
             [n (rootnode-visits a)]
             [v (rootnode-score a)]
             [U (UCB v N n)])
        (cond [(equal? U 'infi) (set! max U)
                                (set! maxChild a)]
               [(> U max)   (set! max U)
                           (set! maxChild a)
                           (helper (cdr l))])
        )]))
  (helper childList)
  maxChild)

(define (myexpand Board mydepth move)
    (if (>= mydepth depth) (get-score Board)
        (myexpand (car (get-move 81 Board move)) (+ 1 mydepth) (alter move))))
    
(define (montecarlo2 S)
  (define mydepth (rootnode-mydepth S))
  (define childVec (rootnode-childVec S))
  (define (exp&back-propogate Si)
     (let* ([myscor (myexpand (rootnode-Board Si)
                         (rootnode-mydepth Si)
                         (rootnode-move Si))])
       (set-rootnode-score! Si (+ myscor (rootnode-score Si)))
       (set-rootnode-visits! Si (+ 1 (rootnode-visits Si)))
       (vector-set! childVec (rootnode-No Si) Si)))
  (cond [(equal? mydepth depth)
         (let* ([e (rootnode-score S)]
                [e1 (if (= e 0) 
                       (get-score (rootnode-Board S)) e
                       )])
           (set-rootnode-score! S e1)
           e1)]
        [else
  (define N (rootnode-visits S))
  (set-childvec S)
  (set! childVec (rootnode-childVec S))
  (define Si (get-selected childVec N))
  (if (is-leaf? Si)
      (begin (exp&back-propogate Si)
             (let ([e (rootnode-score Si)])
               (set-rootnode-score! S (+ e (rootnode-score S)))
               (set-rootnode-visits! S (+ 1 (rootnode-visits S)))
               e))
      (let ([e (montecarlo2 Si)])
        (vector-set! childVec (rootnode-No Si) Si)
        (set-rootnode-score! S (+ e (rootnode-score S)))
        (set-rootnode-visits! S (+ 1 (rootnode-visits S)))
        (set-rootnode-childVec! S childVec)
        e))]))

(define (montecarlo S)
  (set-childvec S)
  (define abort-time (+ (current-seconds) 5))
  (define (loop n)
    (if (or (= n 0) (equal? (current-seconds) abort-time)) void
        (begin
    (montecarlo2 S)
    (loop (- n 1)))))
  (loop frequency)
  (get-scorable (rootnode-childVec S) (rootnode-move S)))

(define (get-scorable childVec move)
  (define childList (vector->list childVec))
  (define max/min (rootnode-score (car childList)))
  (define Max/Minplayer (car childList))
  (define f (if (equal? move 'black) > <))
  (define (helper l)
    (if (null? l) void
    (let* ([a (car l)]
          [score (rootnode-score a)])
      (if (f score max/min)
          (begin (set! max/min score) (set! Max/Minplayer a) (helper (cdr l)))
          (helper (cdr l))))))
  (helper childList)
  (rootnode-posn Max/Minplayer))
  
 (define (get-score Board-Vect)
   (define C 4)
   (let* ([l (board-vect->board-list Board-Vect)]
         [black-stones (counts 'black l)]
         [white-stones (counts 'white l)]
         [p (car (g:territories Board-Vect))]
         [black-points (length (hash-ref p 'black )) ]
         [white-points (length (hash-ref p 'white ))]
         [q (helper Board-Vect)]
         [black-safe (+ (car q) (caddr q))]
         [white-safe (+ (cadr q) (cadddr q))]
         [black-score (+ black-stones black-points
                         (* (- C 1) black-safe))]
         [white-score (+ white-stones white-points
                         (* (- C 1) white-safe))])
     (- black-score white-score)))


 
(define (helper Board-Vect)
  (define myboardgroups (g:set&getbgroups Board-Vect 'test))
  (define black-removed-chains '())
  (define white-removed-chains '())
  (define territories (g:territories Board-Vect ))
  (define black-ter-groups (cadr territories))
  (define white-ter-groups (caddr territories))
  (define (remove-chains)
    (define (helper chain-list acc p)
      (if (null? chain-list) (cons acc p)
      (let* ([a (car chain-list)]
            [n (if (equal? (cdr a) 'black)
                   (begin ;(displayln "hi")
                          (no-of-vitals (car a) black-ter-groups))
                   (no-of-vitals (car a) white-ter-groups))])
        (if (< n 2)
            (begin
              (if (equal? (cdr a) 'black)
              (set! black-removed-chains (cons a black-removed-chains))
              (set! white-removed-chains (cons a white-removed-chains)))
              (helper (cdr chain-list) acc #t))
            (helper (cdr chain-list) (cons a acc) p)))))
    (define conslp (helper myboardgroups '() #f))
    (set! myboardgroups (car conslp))
    (cdr conslp))
  (define (remove-regions)
    (define symbol 'black)
    (define (helper region-list acc p )
      (if (null? region-list) (cons acc p)
      (let* ([a (car region-list)]
             [p (if (equal? symbol 'black)
                    (removable (car a) black-removed-chains)
                    (removable (car a) white-removed-chains))])
        (if p
            (helper (cdr region-list) acc #t )
            (helper (cdr region-list) (cons a acc) p )))))
    (define conslp (helper black-ter-groups '() #f))
     (set! black-ter-groups (car conslp))
     (set! symbol 'white)
    (define conslp2 (helper white-ter-groups '() #f))
    (set! white-ter-groups (car conslp2))
    (or (cdr conslp) (cdr conslp2)))
  (define (loop)
    (let* ([a (remove-chains)]
          [b (if a (remove-regions) #f)])
      (if  b (loop) void)))
  (loop)
  (define (safe-blocks groups)
    (define safe-blacks 0)
    (define safe-whites 0)
    (define (helper l)
      (cond [(not (null? l))
             (let ([a (car l)])
               (if (equal? (cdr a) 'black)
                   (set! safe-blacks (+ safe-blacks (length (car a))))
                   (set! safe-whites (+ safe-whites (length (car a)))))
               (helper (cdr l)))]))
    (helper groups)
    (cons safe-blacks safe-whites))
  (define (ter-no ter-group)
    (define (helper l i)
      (cond [(null? l) i]
            [else (let ([a (caar l)])
                       (helper (cdr l) (+ i (length a))))]))
    (helper ter-group 0))
  ;(list myboardgroups black-ter-groups white-ter-groups)
  (let ([e (safe-blocks myboardgroups)])
    (list (car e) (cdr e)
          (ter-no black-ter-groups)
          (ter-no white-ter-groups))))

(define (is-point-liberty point achain)
      (define neighs (get-neighbours (car point) (cdr point)))
      (define (helper l)
        (cond [(null? l) #f]
              [else (let ([a (car l)])
                      (if (member a achain)
                          #t
                          (helper (cdr l))))]))
      (helper neighs))

(define (vital achain aregion)
    (define l (map (lambda (x) (is-point-liberty x achain)) aregion))
  (if (> (counts #f l) 0) #f #t))

(define (no-of-vitals achain list-of-regions)
  (define l (map (lambda (x) (vital achain (car x))) list-of-regions))
  (counts #t l))

(define (connected group chain)
  (define l (map (lambda (x) (is-point-liberty x chain)) group))
  (if (> (counts #t l) 0) #t #f))

(define (removable group list-of-chain )
  (define l (map (lambda (x) (connected group (car x))) list-of-chain))
  (if (> (counts #t l) 0) #t #f))

(define (make-rootnode Board move)
  (rootnode 0 Board 0 move 0 0 0 '()))
  
  