#lang racket
(provide update&get  init  legal
         territories    count-score board-vect
         full-redo full-undo redo undo init-history
         can-undo can-redo  set&getbgroups
         undo-added-points)
(require 2htdp/image 2htdp/universe lang/posn)
(require "lc.rkt")
;;;;;;;;;;;;;;;;;;GENERAL PURPOSE;;;;;;;;;;;;
(define (alter move)
  (if (equal? move 'white) 'black 'white))
(define (make-2d-vector r c val)
  (build-vector r (lambda (x) (make-vector c val))))
(define (vector-2d-ref vect r c)
  (vector-ref (vector-ref vect r) c))
(define (vector-2d-set! vect r c val)
  (let ([e (vector-ref vect r)])
    (vector-set! e c val)))
(define (2dvect->list vect)
  (let ([l (build-list (vector-length vect) values)])
  (lc (vector-2d-ref vect x y)
      : x <- l
        y <- l)))
(define (board-vect->board-list board-vect)
  (map car (2dvect->list board-vect) ))
      
;
(define (2d-vector-copy vect)
        (build-vector 9
     (lambda (x) (let ([e (vector-ref vect x)])
                   (build-vector 9 (lambda (y) (vector-ref e y)))))))

;;
(define (counts a l)
  (define (helper a l i)
    (if (null? l) i
    (if (equal? a (car l)) (helper a (cdr l) (+ i 1)) (helper a (cdr l) i))))
  (helper a l 0))

(define (sum l)
  (define (helper sum l)
    (if (null? l) sum (helper (+ sum (car l)) (cdr l))))
  (helper 0 l))

(define (remove-all-null l)
  (foldl
   (lambda (x y)
     (cond [(null? x) y]
           [else (cons x y)]))
   '()  l))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-j 0)
(define (init)
  (set! board-vect (make-2d-vector 9 9 (cons 'none 4)))
  (set! board-groups '())
  (set! undo-added-points '())
  (set! undo-removed-groups '()))
;;;;;;;;;;;;;;IMPORTANT;;;;;;;;;;;;;;;;;;
(define menu-xs (cons 221 316))
(define menu-ys (cons 115 154))
(define board-xs (cons 31 509))
(define board-ys (cons 247 724))
(define boundary-ys (cons 207 753))
(define cell-width 60)
(define cell-height 60)
(define (n->x n) ;n will be from 0 to 8
  (+ (car board-xs) (* n cell-width)))
(define (n->y n)
  (- (cdr board-ys) (* n cell-height)))
(define black-move (circle 30 'solid "black"))
(define white-move (circle 30 'solid "white"))
(define (boardvect->list board-vect)
  (remove-all-null
  (lc (let ([val (car (vector-2d-ref board-vect nx ny ))])
        (cond [(equal? val 'black) (cons (make-posn (n->x nx) (n->y ny)) black-move)]
              [(equal? val 'white) (cons (make-posn (n->x nx) (n->y ny)) white-move)]
              [else '()]))
      : nx <- (build-list 9 values)
        ny <- (build-list 9 values)
        )))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define board-vect
  (make-2d-vector 9 9 (cons 'none 4)))
(define stack-board-vect
  (make-2d-vector 9 9 (cons 'none 4)))
(define old-board-vect
  (make-2d-vector 9 9 (cons 'none 4)))
(define test-board-vect
  (make-2d-vector 9 9 (cons 'none 4)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-liberty nx ny)
  (cdr (vector-2d-ref board-vect nx ny)))
(define (set-liberty! nx ny val [symbol 'none])
  (define  this-vect
  (if (equal? symbol 'none) board-vect test-board-vect))
  (let ([e (vector-2d-ref this-vect nx ny)])
    (if (equal? symbol 'none)
  (vector-2d-set! board-vect nx ny (cons (car e) val))
  (vector-2d-set! test-board-vect nx ny (cons (car e) val)))))
(define (add-board-vect nx ny sym)
  (vector-2d-set! board-vect nx ny (cons sym 4)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (calculate&set-liberties nx ny [symbol 'none])
  (define this-vect
  (if (equal? symbol 'none)  board-vect test-board-vect))
  (let ([e1 (if (> nx 0)
                (car (vector-2d-ref this-vect (- nx 1) ny))
                'one)]
        [e2 (if (< nx 8)
                (car (vector-2d-ref this-vect (+ nx 1) ny))
                'one)]
        [e3 (if (> ny 0)
                (car (vector-2d-ref this-vect nx (- ny 1)))
                'one)]
        [e4 (if (< ny 8)
                (car (vector-2d-ref this-vect nx (+ ny 1)))
                'one)])
      (set-liberty! nx ny (counts 'none (list e1 e2 e3 e4)) symbol)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-liberties nx ny [symbol 'none])
    (begin
      (calculate&set-liberties nx ny symbol)
      (if (> nx 0)
      (calculate&set-liberties (- nx 1) ny symbol)
      void)
      (if (< nx 8)
      (calculate&set-liberties (+ nx 1) ny symbol)
      void)
      (if (> ny 0)
      (calculate&set-liberties nx (- ny 1) symbol)
      void)
      (if (< ny 8)
      (calculate&set-liberties nx (+ ny 1) symbol)
      void)
      ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define board-groups '())
(define test-groups '())
(define (get-group-liberties list-of-posn [symbol 'none]) ;needs update
  (define this-vect
  (if (equal? symbol 'none)  board-vect test-board-vect))
 (sum (map (lambda (x)
         (cdr (vector-2d-ref this-vect (car x) (cdr x))))
       list-of-posn)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (add-in-group nx ny move [symbol 'none])
  (define this-vect
  (if (equal? symbol 'none)  board-vect test-board-vect))
  (define my-groups
  (if (equal? symbol 'none)  board-groups test-groups))
 (define e1 (if (> nx 0)
                (car (vector-2d-ref this-vect (- nx 1) ny))
                'none))
 (define e2 (if (< nx 8)
                (car (vector-2d-ref this-vect (+ nx 1) ny))
                'none))
 (define e3 (if (> ny 0)
                (car (vector-2d-ref this-vect nx (- ny 1)))
                'none))
 (define e4 (if (< ny 8)
                (car (vector-2d-ref this-vect nx (+ ny 1)))
                'none))
 (define group1
   (cond [(equal? e1 move)
            (global-helper my-groups (- nx 1) ny '() symbol)
          ]
         [else '()]))
  (set! my-groups
  (if (equal? symbol 'none)  board-groups test-groups))
  (define group2
   (cond [(equal? e2 move)
            (global-helper my-groups (+ nx 1) ny '() symbol)
          ]
         [else '()]))
  (set! my-groups
  (if (equal? symbol 'none)  board-groups test-groups))
  (define group3
   (cond [(equal? e3 move)
            (global-helper my-groups nx (- ny 1) '() symbol)
          ]
         [else '()]))
  (set! my-groups
  (if (equal? symbol 'none)  board-groups test-groups))
  (define group4
   (cond [(equal? e4 move)
            (global-helper my-groups nx (+ ny 1) '() symbol)
          ]
         [else '()]))
  
  (define this-group  (cons (cons (cons nx ny)
                                  (remove-duplicates
                            (append group1 group2 group3 group4))) move))
  (if (equal? symbol 'none)
      (set! board-groups (cons this-group board-groups))
      (set! test-groups (cons this-group test-groups)))
  )

(define (remove-groups-of-opponent nx ny move [symbol 'none])
  (define removel-g '())
  (define move1 (if (equal? move 'black) 'white 'black))
(define this-vect
  (if (equal? symbol 'none)  board-vect test-board-vect))
  (define my-groups
  (if (equal? symbol 'none)  board-groups test-groups))
 (define e1 (if (> nx 0)
                (car (vector-2d-ref this-vect (- nx 1) ny))
                'none))
 (define e2 (if (< nx 8)
                (car (vector-2d-ref this-vect (+ nx 1) ny))
                'none))
 (define e3 (if (> ny 0)
                (car (vector-2d-ref this-vect nx (- ny 1)))
                'none))
 (define e4 (if (< ny 8)
                (car (vector-2d-ref this-vect nx (+ ny 1)))
                'none))
 (define group1
(let* ([e
          (cond [(equal? e1 move1)
                   (global-helper my-groups (- nx 1) ny '() symbol)]
                [else '()]) ]
       [p (equal? 0 (get-group-liberties e symbol))])
       (if p (begin (set! removel-g (cons e removel-g))'())
                          (cons e move1))))
  (set! my-groups
  (if (equal? symbol 'none)  board-groups test-groups))
(define group2
(let* ([e
          (cond [(equal? e2 move1)
                   (global-helper my-groups (+ nx 1) ny '() symbol)]
                [else '()]) ]
       [p (equal? 0 (get-group-liberties e symbol))])
       (if p (begin (set! removel-g (cons e removel-g))'())
                          (cons e move1))))
  (set! my-groups
  (if (equal? symbol 'none)  board-groups test-groups))
(define group3
(let* ([e
          (cond [(equal? e3 move1)
                   (global-helper my-groups nx (- ny 1) '() symbol)]
                [else '()]) ]
       [p (equal? 0 (get-group-liberties e symbol))])
       (if p (begin (set! removel-g (cons e removel-g))'())
                          (cons e move1))))
  (set! my-groups
  (if (equal? symbol 'none)  board-groups test-groups))
(define group4
(let* ([e
          (cond [(equal? e4 move1)
                   (global-helper my-groups nx (+ ny 1) '() symbol)]
                [else '()]) ]
       [p (equal? 0 (get-group-liberties e symbol))])
       (if p (begin (set! removel-g (cons e removel-g))'())
                          (cons e move1))))
  
  (define this-group (remove-duplicates (cons group1
                     (cons group2  (cons group3
                     (cons group4  '()))))))
  (if (equal? symbol 'none)
 (set! board-groups
       (remove-all-null
       (append this-group board-groups)))
 (set! test-groups
       (remove-all-null
       (append this-group test-groups))))
;  (displayln "opponent g1 g2 g3 g4")
;  (displayln group1) (displayln group2) (displayln group3) (displayln group4)
  
  removel-g)

(define (remove-groups-of-self nx ny move [symbol 'none])
  (define removel-g '())
(define this-vect
  (if (equal? symbol 'none)  board-vect test-board-vect))
  (define my-groups
  (if (equal? symbol 'none)  board-groups test-groups))
 (define e1 (if (> nx 0)
                (car (vector-2d-ref this-vect (- nx 1) ny))
                'none))
 (define e2 (if (< nx 8)
                (car (vector-2d-ref this-vect (+ nx 1) ny))
                'none))
 (define e3 (if (> ny 0)
                (car (vector-2d-ref this-vect nx (- ny 1)))
                'none))
 (define e4 (if (< ny 8)
                (car (vector-2d-ref this-vect nx (+ ny 1)))
                'none))
 (define group
(let* ([e  (global-helper my-groups nx ny '() symbol)]
       [p (equal? 0 (get-group-liberties e symbol))])
       (if p (begin (set! removel-g (cons e removel-g))'())
                          (cons e move))))
  (set! my-groups
  (if (equal? symbol 'none)  board-groups test-groups))
 (define group1
(let* ([e
          (cond [(equal? e1 move)
                   (global-helper my-groups (- nx 1) ny '() symbol)]
                [else '()]) ]
       [p (equal? 0 (get-group-liberties e symbol))])
       (if p (begin (set! removel-g (cons e removel-g))'())
                          (cons e move))))
  (set! my-groups
  (if (equal? symbol 'none)  board-groups test-groups))
(define group2
(let* ([e
          (cond [(equal? e2 move)
                   (global-helper my-groups (+ nx 1) ny '() symbol)]
                [else '()]) ]
       [p (equal? 0 (get-group-liberties e symbol))])
       (if p (begin (set! removel-g (cons e removel-g))'())
                          (cons e move))))
  (set! my-groups
  (if (equal? symbol 'none)  board-groups test-groups))
(define group3
(let* ([e
          (cond [(equal? e3 move)
                   (global-helper my-groups nx (- ny 1) '() symbol)]
                [else '()]) ]
       [p (equal? 0 (get-group-liberties e symbol))])
       (if p (begin (set! removel-g (cons e removel-g))'())
                          (cons e move))))
  (set! my-groups
  (if (equal? symbol 'none)  board-groups test-groups))
(define group4
(let* ([e
          (cond [(equal? e4 move)
                   (global-helper my-groups nx (+ ny 1) '() symbol)]
                [else '()]) ]
       [p (equal? 0 (get-group-liberties e symbol))])
       (if p (begin (set! removel-g (cons e removel-g))'())
                          (cons e move))))
  (define this-group (remove-duplicates (cons group   (cons group1
                     (cons group2  (cons group3
                     (cons group4  '())))))))
  (if (equal? symbol 'none)
 (set! board-groups
       (remove-all-null
       (append this-group board-groups)))
 (set! test-groups
       (remove-all-null
       (append this-group test-groups))))
;  (displayln "self g0 g1 g2 g3 g4") (displayln group)
;  (displayln group1) (displayln group2) (displayln group3) (displayln group4)
  removel-g
  )

(define (set-board-vect list-of-posn [symbol 'none])
  (if (equal? symbol 'none)
  (map (lambda (x)
        (vector-2d-set! board-vect (car x) (cdr x)
                        (cons 'none 4)))
       list-of-posn)
  (map (lambda (x)
        (vector-2d-set! test-board-vect (car x) (cdr x)
                        (cons 'none 4)))
       list-of-posn))
  (map (lambda (x)
        (update-liberties (car x) (cdr x) symbol))
       list-of-posn)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update&get nx ny move-sym )
  (set! current-j (+ current-j 1))
  (set! undo-redo-j (+ undo-redo-j 1))
  (add-addedpoint nx ny move-sym )
  (set! old-board-vect (2d-vector-copy stack-board-vect))
  (set! stack-board-vect (2d-vector-copy  board-vect))
  (add-board-vect nx ny move-sym) 
  (update-liberties nx ny)
  (add-in-group nx ny move-sym)
  (let* ([e1 (begin
               (remove-groups-of-opponent nx ny move-sym)
              )]
        [justforsake (begin
                       (set-board-vect (append* (remove-all-null e1))))]
         [e2  (begin 
               (remove-groups-of-self nx ny move-sym))])
    (add-removedgroups (append* (remove-all-null e1)) (alter move-sym))
  (set-board-vect  (append* (remove-all-null e2))))
  (displayln "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
  (displayln "this is board-groups")
  (displayln board-groups)
  (newline)
  (boardvect->list board-vect)
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;GLOBAL-HELPER;;;;;;;;;;;;;;;;;;
    (define (global-helper l nx ny  acc [symbol 'none])
    (cond [(null? l) '()]
          [else (let* ([e  (car l)]
                      [p (if (null? e) #f
                                (member (cons nx ny) (car e)))])
                  (if (equal? p #f)
                      (global-helper (cdr l) nx ny (cons e acc) symbol)
                      (begin
                        (if (equal? symbol 'none)
                            (begin
                        (if (null? acc)
                           
                      (set! board-groups (cdr l))    
                      (set! board-groups (append (cdr l) acc)))
                        )
                        (if (null? acc)
                      (set! test-groups (cdr l))    
                      (set! test-groups (append (cdr l) acc))))
                      (car e))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (legal nx ny move [myboardvect board-vect])
  (init-test)
  (let* ([e (vector-2d-ref myboardvect nx ny)]
        [g
    (if (equal? (car e) 'none)
        (check-further nx ny move myboardvect)
        (cons #f test-board-vect))])
    (init-test)
    g))

(define (check-further nx ny move myboardvect)
  (set! test-board-vect
        (2d-vector-copy myboardvect))
  (set&getbgroups test-board-vect 'test)
  (vector-2d-set! test-board-vect nx ny (cons move 4))
  (update-liberties nx ny 'test)
  (add-in-group nx ny move 'test)
  (let* ([e1 (remove-all-null (begin
               (remove-groups-of-opponent nx ny move 'test)
              ))]
        [justforsake (begin
                       ;(newline)
;    (displayln "test-board-groups2")
 ;   (displayln test-groups)
                       (set-board-vect (append* (remove-all-null e1)) 'test))]
         [e2  (remove-all-null
               (begin
       
               (remove-groups-of-self nx ny move 'test)))])
   ;  (newline)
;(displayln "e1 and e2")
 ;                           (displayln e1) (displayln e2)
  ;  (displayln "test-board-groups3")
   ; (displayln test-groups)
    (cond [(not (null? e2)) (begin  ;(display 'fault)
                              (cons #f test-board-vect))]
          [else (begin            
                (set-board-vect  (append* (remove-all-null e2)) 'test)
                (cons (not (equal? test-board-vect stack-board-vect))
                      test-board-vect))])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (init-test)
  (set! test-board-vect
       (make-2d-vector 9 9 (cons 'none 4)))
  (set! test-groups '()))

;;;;;;;;SOME USEFUL POSES;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sucide)
  (update&get 1 8 'black)
  (update&get 2 8 'white)
  (update&get 1 7 'black)
  (update&get 2 7 'white)
  (update&get 0 7 'black)
  (update&get 0 6 'white)
  (update&get 3 8 'black)
  (update&get 1 6 'white)
  ) ;then (0,8) black
(define (sucide2)
  (add-board-vect 1 8 'black)
  (add-board-vect 2 8 'white)
  (add-board-vect 1 7 'black)
  (add-board-vect 2 7 'white)
  (add-board-vect 0 7 'black)
  (add-board-vect 0 6 'white)
  (add-board-vect 3 8 'black)
  (add-board-vect 1 6 'white)
  )
(define (ko)
  (update&get 2 8 'black)
  (update&get 3 8 'white)
  (update&get 1 7 'black)
  (update&get 4 7 'white)
  (update&get 2 6 'black)
  (update&get 3 6 'white)
  (update&get 3 7 'black))
;then (2 7) white ,(3 7) black
;;;;;;;;;;;;;;;SCORING;;;;;;;;;;;;;;;;;;;;;
(define (count-score)
  (let* ([just (remove-dead-groups)]
         [l (board-vect->board-list board-vect)]
         [black-stones (counts 'black l)]
         [white-stones (counts 'white l)]
         [p (car (territories))]
         [black-points (length (hash-ref p 'black )) ]
         [white-points (length (hash-ref p 'white ))]
         ;;;;;;;;;;;;
         [black-score (+ black-points black-stones)]
         [white-score (+ white-points white-stones)])
    (- black-score (+ white-score 7.5))))
;;;;;;;;;REMOVING DEAD-STONES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (remove-dead-groups) void)
;;;;TERRITORY COUNT USING BREDTH FIRST SEARCH;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (territories [myboardvect board-vect])
  (define this-vector (2d-vector-copy myboardvect))
  (define list-of-white-ters '())
  (define list-of-black-ters '())
  (define (init) (begin
                       (set! this-vector (2d-vector-copy myboardvect))
                       (cond [(equal? tag 'white)
                              (set! list-of-white-ters (cons (cons current-group tag) list-of-white-ters))]
                             [(equal? tag 'black)
                              (set! list-of-black-ters (cons (cons current-group tag) list-of-black-ters))])
                       (set! tag 'none)
                       (set! current-group '())))
  (define unexplored-list (remove-all-null
                     (lc (if (equal? 'none (car (vector-2d-ref myboardvect x y)))
                            (cons x y)
                            '())
                        : x <- (build-list 9 values)
                          y <- (build-list 9 values))))
  (define hash-territory (make-hash (list (cons 'black '()) (cons 'white '()))))
  (define current-group '())
  (define tag 'none) ;n means don't know till
                  ;b means till-now black ,w means till-now white
                  ;and d means dame
  (define (add-in-group a) ; at the end
    (set! current-group (append current-group (list a))))
  
  (define queue '())
  (define (top) (if (null? queue) (error "Queue is empty")
                    (car queue)))
  (define (pop) (set! queue (cdr queue)))
  (define (add-queue a) ; at the end
    (set! queue (append queue (list a))))
  (define (get-neighbours nx ny)
  (let ([e1 (if (> nx 0) (cons (- nx 1) ny) '())]
        [e2 (if (< nx 8) (cons (+ nx 1) ny) '())]
        [e3 (if (> ny 0) (cons nx (- ny 1)) '())]
        [e4 (if (< ny 8) (cons nx (+ ny 1)) '())])
    (remove-all-null (list e1 e2 e3 e4))))
  (define (set-tag! tag2)
    (cond [(or (equal? tag 'dame)
               (equal? tag tag2)) void]
          [(equal? tag 'none) (set! tag tag2)]
          [else (set! tag 'dame)]))   
  (define (helper point)
        (let* ([x (car point)]
               [y (cdr point)]
              [tag2  (vector-2d-ref this-vector x y)])
         ; (displayln tag2)
       (cond [(equal? tag2 'e) void]
             [else (begin
                         (vector-2d-set! this-vector x y 'e)
                         (set! unexplored-list (remove point unexplored-list))
                         (cond [(equal? (car tag2) 'none) (begin
                                                   (map add-queue (get-neighbours x y))
                                                   (add-in-group point))]
                               [else (set-tag! (car tag2))])
;                               [(equal? tag2 'black) (set ]
;                               [(equal? tag2 'white)  ]
;                               [else (error "tag is not known")]))]
                               )])))
  (define (execute)
;    (displayln queue) (displayln "&&&&&&&&&&&&&")
;   (displayln "this-is-tag") (displayln tag)
;   (displayln "this-is-len") (displayln (length current-group))
    (cond [(null? queue) (cond [(equal? tag 'dame) (begin
                                                     (init)
;                                                     (displayln "@@@@@@@@@@@@@@@@")
;                                                     (displayln hash-territory)
                                                     )]
                               [(equal? tag 'none) (set! unexplored-list '())]
                               [else (let ([e (hash-ref hash-territory tag)])
                               (hash-set! hash-territory tag (append e current-group))
                               (init)
;                               (displayln "@@@@@@@@@@@@@@@@")
;                               (displayln hash-territory)
                                       )])]
          [else (begin (helper (top)) (pop) (execute))]))
  (define (propogate)
   ; (displayln unexplored-list) (displayln "&&&&&&&&&&&&&")
    (cond [(null? unexplored-list) void]
          [else (begin
                  (let ([e (car unexplored-list)])
                    (begin (set! unexplored-list (remove e unexplored-list))
                           (set! queue (list e))
                           (execute)))
                           (propogate))]))
  (propogate)
  (list hash-territory list-of-black-ters list-of-white-ters))
        
;;;;;;;;;;KEEP TRACK OF HISTORY;;;;;;;;;;;;;;;;;;;;;;;
;;WE HAVE ASSUMED THAT ALL FUNCTION WILL BE CALLED IF THEY CAN BE CALLED
(define (can-undo) (> undo-redo-j 0))
(define (can-redo) (> current-j undo-redo-j))
(define i 0)
(define undo-redo-j 0)
(define full-redo-vector '())
(define full-redo-groups '())
(define full-redo-stack '())
(define full-redo-old '())
(define undo-added-points '()) 
(define undo-removed-groups '())
(define redo-added-points '())
(define redo-removed-groups '())
(define (init-history)
  (set! current-j undo-redo-j)
  (set! redo-added-points '())
  (set! redo-removed-groups '())
  
  (set! i 0)
  (set&getbgroups board-vect)
  (if (even? undo-redo-j)
      'black 'white))

(define (add-addedpoint nx ny move )
  (set! undo-added-points (cons (list nx ny move) undo-added-points)))
(define (add-removedgroups e-opponent move)
  (set! undo-removed-groups (cons (cons e-opponent move) undo-removed-groups)))


(define (redo)
  (set! undo-redo-j (+ undo-redo-j 1))
  (let ([rem-p (car redo-added-points)]
        [add-group (car redo-removed-groups)])
  (set! redo-added-points (cdr redo-added-points))
  (set! redo-removed-groups (cdr redo-removed-groups))
  (set! undo-added-points (cons rem-p undo-added-points))
  (set! undo-removed-groups (cons add-group undo-removed-groups))
  (set! old-board-vect (2d-vector-copy stack-board-vect))
  (set! stack-board-vect (2d-vector-copy  board-vect))
    (vector-2d-set! board-vect (car rem-p) (cadr rem-p) (cons (caddr rem-p) 4))
    (set! board-vect (vector+-group add-group board-vect 'rem))
    (boardvect->list board-vect)))

(define (bgroups->bvect agroup) ;list ,every element is cons list-of-posn move
  (define this-vector (make-2d-vector 9 9 (cons 'none 4)))
  (map
   (lambda (pair)
     (let ([l (car pair)]
           [move (cdr pair)])
       (map
        (lambda (x)
       (vector-2d-set! this-vector (car x) (cdr x) (cons move 4)))
        l)))
   agroup)
  this-vector)


(define (undo)
  (set! undo-redo-j (- undo-redo-j 1))
  (do-full-set i)
  (set! board-vect (2d-vector-copy stack-board-vect))
  (set! stack-board-vect (2d-vector-copy old-board-vect))
  (let ([add-p (car undo-added-points)]
        [rem-group (car undo-removed-groups)])
  (set! undo-added-points (cdr undo-added-points))
  (set! undo-removed-groups (cdr undo-removed-groups))
  (set! redo-added-points (cons add-p redo-added-points))
  (set! redo-removed-groups (cons rem-group redo-removed-groups))
  (if (< (length  undo-added-points) 2)
      (set! old-board-vect (make-2d-vector 9 9 (cons 'none 4)))
  (let ([add-p2 (cadr undo-added-points)]
        [rem-group2 (cadr undo-removed-groups)])
       (set! old-board-vect (vector+-group rem-group2 old-board-vect 'add))
       (vector-2d-set! old-board-vect (car add-p2) (cadr add-p2) (cons 'none 4))
  (set! i (+ i 1))
;  (newline)
;  (displayln "-----------------------------------------")
;  (displayln "added-points")
;  (displayln add-p2)
;    (displayln "renoved-groups")
;    (displayln rem-group2)
;  (displayln "board-vect")
;  (displayln board-vect)
;  (displayln "@@@@@@@@@@@@@@@@@@")
;  (displayln "stack-board-vect")
;  (displayln stack-board-vect)
;  (displayln "@@@@@@@@@@@@@@@@@@")
;  (displayln "old-board-vect")
;  (displayln old-board-vect)
    ))
    (boardvect->list board-vect)
  ))

(define (vector+-group pair gvector symbol)
  (define this-vector (2d-vector-copy gvector))
  (cond [(equal? symbol 'rem)
                (if (null? pair)
                void
               (let ([l (car pair)]
                     [move (cdr pair)])
                   (map
                        (lambda (x)
                        (vector-2d-set! this-vector (car x) (cdr x) (cons 'none 4)))
                    l)))]
        [(equal? symbol 'add)
                (if (null? pair)
                void
               (let ([l (car pair)]
                     [move (cdr pair)])
                   (map
                        (lambda (x)
                        (vector-2d-set! this-vector (car x) (cdr x) (cons move 4)))
                    l)))])
  this-vector)

(define (full-undo)
  (set! undo-redo-j 0)
  (do-full-set i)
  (set! board-vect (make-2d-vector 9 9 (cons 'none 4)))
  (set! stack-board-vect (make-2d-vector 9 9 (cons 'none 4)))
  (set! old-board-vect (make-2d-vector 9 9 (cons 'none 4)))
  (set! redo-added-points
        (append (reverse undo-added-points) redo-added-points))
  (set! redo-removed-groups
        (append (reverse undo-removed-groups) redo-removed-groups))
  (set! undo-added-points '()) 
  (set! undo-removed-groups '())
  (set! i (+ i 1))
  (boardvect->list board-vect))

(define (full-redo)
  (set! undo-redo-j current-j)
  (set! board-vect (2d-vector-copy full-redo-vector))
  (set! stack-board-vect (2d-vector-copy full-redo-stack))
  (set! old-board-vect (2d-vector-copy full-redo-old))
  (set! undo-added-points
        (append  (reverse redo-added-points) undo-added-points))
  (set! undo-removed-groups
        (append (reverse redo-removed-groups) undo-removed-groups))
  (set! redo-added-points '()) 
  (set! redo-removed-groups '())
  (boardvect->list board-vect))

(define (do-full-set i)
  (cond [(= i 0)
  (set! full-redo-vector (2d-vector-copy  board-vect) )
  (set! full-redo-groups board-groups)
  (set! full-redo-stack (2d-vector-copy  stack-board-vect))
  (set! full-redo-old (2d-vector-copy  old-board-vect))]))


(define (set&getbgroups vect [symbol 'none])
  (if (equal? symbol 'none)
      (set! board-groups '())
      (set! test-groups '()))
  (cond [(not (equal? symbol 'none))
         (set! test-board-vect vect)])
  (define l (build-list 9 values))
 (lc
    (let* ([move (car (vector-2d-ref vect nx ny ))])
      (cond [(not (equal? move 'none))
;             (displayln "------------------------------------------------------")
;             (displayln test-groups)
;             (displayln test-board-vect)
             (add-in-group nx ny move symbol)]))
  : nx <- l   ny <- l)
  (if (equal? symbol 'none)
      board-groups test-groups))
   
