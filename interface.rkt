#lang racket
(require 2htdp/image 2htdp/universe lang/posn)
(require (prefix-in g: "groups3.rkt")
         (prefix-in m: "montecarlo1.rkt"))
;;;;;;;;;;;;;GLOBALS;;;;;;;;;;;;
(define WIDTH 540)
(define HEIGHT (/ 1845 2))
(define background (empty-scene WIDTH HEIGHT))
(define move 'black)
;;;;;;;;;;MAIN-MENU IMAGES AND  CO-ORDINATES;;;;;;;;
(define all-x1 25)
(define all-x2 515)
(define all-midx (cons 255 285))
(define all-height 130)
(define all-gap 30)
(define single-ys (cons 100 230))
(define two-ys (cons 260 400))
(define options-ys (cons 430 560))
(define how-ys (cons 590 730))
(define last-ys (cons 760 922.5))
(define cleaner (scale/xy 520/1080 160/1861 (bitmap/file "picts/cleaner.jpg")))
(define clean-x 270)
(define main-menu
  (place-images
   (list
    cleaner
    cleaner
    )
   (list
    (make-posn clean-x (* 0.5 (+ (car options-ys) (cdr options-ys))))
    (make-posn clean-x (* 0.5 (+ (car last-ys) (cdr last-ys))))
    )
  (scale 0.5
  (bitmap/file "picts/main_menu.jpg"))))

(define main-menu-image-list (list main-menu))
(define main-menu-posn-list (list (make-posn (/ WIDTH 2) (/ HEIGHT 2))))

;;;;;;;;;;BOARD-CIRCLES CO-ORDINATES AND IMAGES;;;;;;;;;;;;
;;I am assuming x left to right and y down to up
(define down-optionys (cons 870 920))
(define full-undoxs (cons 38 90))
(define undoxs (cons 105 158))
(define redoxs (cons 185 237))
(define full-redoxs (cons 252 308))
(define territoryxs (cons 333 415))
(define passxs (cons 424 504))

(define menu-xs (cons 221 316))
(define menu-ys (cons 115 154))
(define board-xs (cons 31 509))
(define board-ys (cons 247 724))
(define boundary-ys (cons 207 753))
(define cell-width 60)
(define cell-height 60)
(define (min-ref l)
  (define min-i 0)
  (define min-value (car l))
  (define (helper l1 i)
    (if (null? l1) void
    (if (< (car l1) min-value)
        (begin
        (set! min-value (car l1))
        (set! min-i i)
        (helper (cdr l1) (+ i 1)))
        (helper (cdr l1) (+ i 1)))))
  (helper (cdr l) 1)
  min-i)


(define (2d-vector-copy vect)
        (build-vector 9
     (lambda (x) (let ([e (vector-ref vect x)])
                   (build-vector 9 (lambda (y) (vector-ref e y)))))))
(define (n->x n) ;n will be from 0 to 8
  (+ (car board-xs) (* n cell-width)))
(define (n->y n)
  (- (cdr board-ys) (* n cell-height)))
(define (xn-nearest x)
  (let ([lx (map (lambda (n) (abs (- (n->x n) x)))
                   (build-list 9 values))])
     (min-ref lx)))
(define (yn-nearest y)
  (let ([ly (map (lambda (n) (abs (- (n->y n) y)))
                   (build-list 9 values))])
   (min-ref ly)))
(define (x-nearest x)
  (let ([lx (map (lambda (n) (abs (- (n->x n) x)))
                   (build-list 9 values))])
    (n->x (min-ref lx))))
(define (y-nearest y)
  (let ([ly (map (lambda (n) (abs (- (n->y n) y)))
                   (build-list 9 values))])
    (n->y (min-ref ly))))
(define black-move (circle 30 'solid "black"))
(define white-move (circle 30 'solid "white"))
(define red-circle (circle 32 'outline (make-pen "red" 5 "solid" "round" "round")))
(define board-image-list '())
(define board-posn-list '())
(define (init-board)
  (set! board-image-list '())
  (set! board-posn-list '())
  (set! moving-animation-image-list '())
  (set! moving-animation-posn-list '())
  (set! moving-animation-posn-list2 '())
  (set! pass-cond 0)
  (set! territory-cond #f)
  (set! undo-redo-state #f)
  (set! move 'black)
  (set! pass-anime #f)
  (g:init))
(define (alter move)
  (if (equal? move 'black) 'white 'black))
(define (add-board x y move)
  (cond [(equal? move 'black)
         (set! board-image-list (cons black-move board-image-list))
         (set! board-posn-list (cons (make-posn x y) board-posn-list))]
        [(equal? move 'white)
         (set! board-image-list (cons white-move board-image-list))
         (set! board-posn-list (cons (make-posn x y) board-posn-list))]))
;;;;;;;;;;;;;;;;;;SINGLE-PLAYING IMAGES AND CO-ORDINATES;;;;;;;;;;;;;;;
(define single-back
  (scale 0.5
  (bitmap/file "picts/single.jpg")))
(define single-image-list (list single-back))
(define single-posn-list (list
                          (make-posn (/ WIDTH 2) (/ HEIGHT 2))))

;;;;;;;;;;;;;;;;;;DOUBLE-PLAYING IMAGES AND CO-ORDINATES;;;;;;;;;;;;;;;
(define double-back
  (scale 0.5
  (bitmap/file "picts/double_player.jpg")))
(define double-image-list (list double-back))
(define double-posn-list (list
                          (make-posn (/ WIDTH 2) (/ HEIGHT 2))))
;;;;;;;ANIMATE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (animate-pass image)
  (if pass-anime
      (place-images (list pass-image) (list center) image)
      image))
(define (animate-last-move image)
  (let ([e g:undo-added-points])
    (if (null? e) image
        (let ([a (car e)])
    (place-images (list red-circle)
                  (list (make-posn (n->x (car a)) (n->y (cadr a))))
                  image)))))
(define (animate-till-time time)
  (define i (current-seconds))
  (define (loop)
    (if (> (- (current-seconds) i) time)
        void
        (loop)))
  (loop))
(define moving-animation-image-list '())
(define moving-animation-posn-list '())
(define moving-animation-posn-list2 '())
(define (line-animate image)
  (cond [(or undo-redo-state (equal? state 'undo-redo-end)) image]
        [else
  (let ([l moving-animation-posn-list2])
    (match l
      ['() image]
      [(list a)
       (let ([x (posn-x a)]
             [y (posn-y a)])
       (add-line
       (add-line image x (car board-ys) x (cdr board-ys)
                 (make-pen "red" 5 "solid" "round" "round"))
       (car board-xs) y (cdr board-xs) y
       (make-pen "red" 5 "solid" "round" "round")))]))]))
;;;;;;;;;;;;;TERRITORIES;;;;;;;;;;;;;;;;;;;
(define territory-cond #f)
(define black-ter-image (circle 10 'solid "black"))
(define white-ter-image (circle 10 'solid "white"))
(define (hash->posn-list color ter-hash)
  (map (lambda (point)
         (make-posn (n->x (car point))
                    (n->y (cdr point)))) (hash-ref ter-hash color)))
(define (animate-ter image)
  ;(displayln territory-cond) 
  (cond [territory-cond (let* ([ter-hash (car (g:territories))]
                               [black-ter-posns (hash->posn-list 'black ter-hash)]
                               [white-ter-posns (hash->posn-list 'white ter-hash)])
                          (place-images
                           (append
                           (make-list  (length black-ter-posns) black-ter-image)
                           (make-list  (length white-ter-posns) white-ter-image))
                           (append
                            black-ter-posns
                            white-ter-posns)
                           image))]
        [else image]))
;;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;
(define end-op-xs (cons 177 360))
(define view-ys (cons 455 509))
(define pl-again-ys (cons 514 570))
(define main-ys (cons 578 627))
(define (win-text)
  (define score (g:count-score))
    (define winner (if (> score 0) "1" "2"))
    (define score1 (number->string (abs score)))
    (overlay
     (text (string-append "Player" winner
                          "  wins by
 " score1 " points!")
           30 "black")
     (rectangle 280 90 'solid "dark goldenrod")))
(define win-text-posn (make-posn 260 375))
(define end-image
  (scale/xy
   359/789
   360/716
  (bitmap/file "picts/end_animation.jpg")))
(define center (make-posn 270 485))
(define pass-image (scale/xy 300/564 70/138 (bitmap/file "picts/pass.jpg")))
;;;;;;;;;;;;;;;;;AI;;;;;;;;;;;;;;;;;
(define (AI-helper)
  (cond [(equal? state 'single)
                              (let* ([p (and (< (g:count-score) 0)
                                             (equal? pass-cond 1))]
                                     [a (if p
                                            'pass
                                            (m:montecarlo
                                         (m:make-rootnode
                                          (2d-vector-copy g:board-vect) move)))])
                                (if (equal? a 'pass)
                                    (cond [(= pass-cond 0)
                                             (set! move (alter move))
                                             (set! pass-anime #t)
                                             (set! pass-cond 1)]
                                          [(= pass-cond 1) (set! state 'end)])
                                    (begin
                                        (add-board (n->x (car a)) (n->y (cdr a)) move)
                                   (let ([boardl (g:update&get (car a) (cdr a) move)])
                                     (set! board-image-list (map (lambda (x) (cdr x)) boardl))
                                     (set! board-posn-list (map (lambda (x) (car x)) boardl)))
                                (set! move (alter move))
                                (set! pass-cond 0))))]))
;;;;;;;;Rules-Images;;;;;;;;;;;;;;;;;;;;
(define (get-image i)
  (scale/xy (/ WIDTH 1080) (/ HEIGHT 1855)
            (bitmap/file (string-append "picts/image" (~a i) ".jpg"))))
(define (next-image image)
  (let ([i (car image)])
    (if (= i 34) image
    (cons (+ i 1) (get-image (+ i 1))))))
(define (last-image image)
      (let ([i (car image)])
          (if (= i 0) image
    (cons (- i 1) (get-image (- i 1))))))
(define rule-image (cons 0 (get-image 0)))
(define rule-backxs (cons 202 337))
(define rule-backys (cons 849 917))
;;;;;;;;;;;;UNDO-REDO;;;;;;;;;;
(define YES-xs (cons 162 252))
(define No-xs (cons 279 368))
(define Y/N-ys (cons 510 563))
(define undo-redo-end-image
  (scale/xy
   359/926
   240/603
  (bitmap/file "picts/undo_redo_end.jpg")))
(define undo-redo-state #f)
(define (every-init-undo-redo)
  (set! moving-animation-image-list '())
  (set! moving-animation-posn-list '())
  (set! undo-redo-state #t))
  
;;;;;;;;;;;;;;;STATE;;;;;;;;;;;;
(define state 'main)
(define state2 'human)
(define pass-anime #f)
(define pass-cond 0)
(define image-list main-menu-image-list)
(define posn-list  main-menu-posn-list)

;;;;;;;;;;;;;;;;DRAW;;;;;;;;;;;;;;
(define (draw nothing)
  (cond [(equal? state 'rules)
         (place-images (list (cdr rule-image))
                       (list center)
                       background)]
        [(equal? state 'undo-redo-end)
    (place-images
   (append
    (list undo-redo-end-image)
    board-image-list
    image-list
    )
   (append
    (list center)
    board-posn-list
    posn-list
    )
    background)
         ]
        [(equal? state 'end)
    (place-images
   (append
    (list (win-text)
     end-image)
    board-image-list
    image-list
    )
   (append
    (list win-text-posn
     center)
    board-posn-list
    posn-list
    )
    background)
         ]
        [(equal? state 'view)
         (animate-ter
  (place-images
   (append 
    board-image-list
    image-list
    )
   (append
    board-posn-list
    posn-list
    )
    background))]
         
        [else
  (line-animate
   (animate-last-move
   (animate-pass
   (animate-ter
  (place-images
   (append
    moving-animation-image-list
    board-image-list
    image-list
    )
   (append
    moving-animation-posn-list
    board-posn-list
    posn-list
    )
    background)))))]))

  
;;;;;;;;BIG BANG ;;;;;;;;;;;;;;;;
(big-bang
    state
  (to-draw  draw)
  (on-tick (lambda (x) (cond [pass-anime (animate-till-time 1) (set! pass-anime #f)])))
  (on-mouse (lambda (nothing x y input)
     (cond
       [(mouse=? input "button-down")
            (cond [(and (equal? state 'rules)
                        (= 34 (car rule-image))
                        (> x (car rule-backxs))
                        (< x (cdr rule-backxs))
                        (> y (car rule-backys))
                        (< y (cdr rule-backys)))
                   (set! image-list main-menu-image-list)
                       (set! posn-list main-menu-posn-list)
                       (set! state 'main)
                       (set! rule-image (cons 0 (get-image 0)))
                     ]
                  [(equal? state 'main)
                        (if (and (> x all-x1)
                                 (< x all-x2))
                            (cond
                                [(and (> y (car single-ys))
                                      (< y (cdr single-ys)))
                                  (set! image-list single-image-list)
                                  (set! posn-list single-posn-list)
                                  (set! state 'single)
                                  (set! state2 'AI)]
                                [(and (> y (car two-ys))
                                      (< y (cdr two-ys)))
                                  (set! image-list double-image-list)
                                  (set! posn-list double-posn-list)
                                  (set! state 'double)
                                  (set! state2 'human)]
                                [(and (> y (car how-ys))
                                      (< y (cdr how-ys)))
                                 (set! state 'rules)]
                                )
                             void)]
                  [(equal? state 'undo-redo-end)
                   (cond
                      [(and (> x (car menu-xs)) (< x (cdr menu-xs))
                            (> y (car menu-ys)) (< y (cdr menu-ys)))
                       (set! image-list main-menu-image-list)
                       (set! posn-list main-menu-posn-list)
                       (set! state 'main)
                       (init-board)]
                      [(and (> y (car Y/N-ys)) (< y (cdr Y/N-ys)))
                       (cond [(and (> x (car YES-xs))
                                   (< x (cdr YES-xs)))
                              (set! move (g:init-history))
                              (set! state (if (equal? state2 'AI) 'single 'double))
                              (cond [(equal? move 'white) (AI-helper)])
                              (set! undo-redo-state #f)]
                             [(and (> x (car No-xs))
                                   (< x (cdr No-xs)))
                              (set! state (if (equal? state2 'AI) 'single 'double))])]
;                   (display "X:") (displayln x)
;                   (display "Y:") (displayln y)
                   )]
                  [(equal? state 'view)
                   (cond
                     [(and (> y (car boundary-ys)) (< y (cdr boundary-ys)))
                      (set! state 'end)]
                     [(and (> y (car down-optionys)) (< y (cdr down-optionys))
                           (> x (car territoryxs)) (< x (cdr territoryxs)))
                              (if territory-cond
                                  (set! territory-cond #f)
                                  (set! territory-cond #t))]
                     [(and (> x (car menu-xs)) (< x (cdr menu-xs))
                            (> y (car menu-ys)) (< y (cdr menu-ys)))
                       (set! image-list main-menu-image-list)
                       (set! posn-list main-menu-posn-list)
                       (set! state 'main)
                       (init-board)]
                     
                    )]
                  [(equal? state 'end)
                   (cond
                      [(and (> x (car end-op-xs))
                            (< x (cdr end-op-xs)))
                       (cond [(and (> y (car view-ys))
                              (< y (cdr view-ys)))
                             (set! state 'view)]
                             [(and (> y (car pl-again-ys))
                                   (< y (cdr pl-again-ys)))
                              (set! state (if (equal? state2 'AI) 'single 'double))
                              (init-board)]
                             [(and (> y (car main-ys))
                                   (< y (cdr main-ys)))
                              (set! image-list main-menu-image-list)
                              (set! posn-list main-menu-posn-list)
                              (set! state 'main)
                              (init-board)])]
                      [else (display x) (display ":") (display y) (newline)]
                   )]
                   [(or (equal? state 'double) (equal? state 'single))
                    ;(display x) (display ":") (display y) (newline)
                    (cond
                      [(and (> x (car menu-xs)) (< x (cdr menu-xs))
                            (> y (car menu-ys)) (< y (cdr menu-ys)))
                       (set! image-list main-menu-image-list)
                       (set! posn-list main-menu-posn-list)
                       (set! state 'main)
                       (init-board)]
                      [ (and  (> y (car boundary-ys)) (< y (cdr boundary-ys))
                              (not undo-redo-state))
                       (let ([x1 (x-nearest x)]
                             [y1 (y-nearest y)]
                             [nx (xn-nearest x)]
                             [ny (yn-nearest y)])
                       (cond [ (car (g:legal nx ny move))
                       (add-board x1 y1 move)
                       (let ([boardl (g:update&get nx ny move)])
                        (set! board-image-list (map (lambda (x) (cdr x)) boardl))
                        (set! board-posn-list (map (lambda (x) (car x)) boardl)))
                       (set! move (alter move))
                       (set! moving-animation-image-list '())
                       (set! moving-animation-posn-list '())
                       (set! moving-animation-posn-list2 '())
                       (set! pass-cond 0)
                       (AI-helper)]))
                       
                         ]
                      [ (and  (> y (car boundary-ys)) (< y (cdr boundary-ys))
                              undo-redo-state)
                        (set! state 'undo-redo-end)
                        ]
                      [(and (> y (car down-optionys)) (< y (cdr down-optionys)))
                       (cond [(and (> x (car territoryxs)) (< x (cdr territoryxs)))
                              (if territory-cond
                                  (set! territory-cond #f)
                                  (set! territory-cond #t))]
                             [(and (> x (car passxs)) (< x (cdr passxs))
                                    (not undo-redo-state))
                              (cond [(= pass-cond 0)
                                     (set! move (alter move))
                                     (set! pass-anime #t)
                                     (set! state (if (equal? state2 'AI) 'single 'double))
                                     (set! pass-cond 1)
                                     (cond [(equal? move 'white)
                                            (AI-helper)])]
                                    [(= pass-cond 1)
                                     (set! state 'end)])]
                            [(and (> x (car full-redoxs)) (< x (cdr full-redoxs)) (g:can-redo))
                              (begin
                                (let ([boardl (g:full-redo)])
                                  (displayln "full-redo")
;                                  (displayln "Boardl")
;                                  (displayln boardl)
                        (set! board-image-list (map (lambda (x) (cdr x)) boardl))
                        (set! board-posn-list (map (lambda (x) (car x)) boardl))))]
                             [(and (> x (car redoxs)) (< x (cdr redoxs)) (g:can-redo))
                              (begin
                                (let ([boardl (g:redo)])
                                  (displayln "redo")
                        (set! board-image-list (map (lambda (x) (cdr x)) boardl))
                        (set! board-posn-list (map (lambda (x) (car x)) boardl))))]
                            [(and (> x (car full-undoxs)) (< x (cdr full-undoxs)) (g:can-undo))
                              (begin
                                (every-init-undo-redo)
                                (let ([boardl (g:full-undo)])
                                  (displayln "full-undo")
                        (set! board-image-list (map (lambda (x) (cdr x)) boardl))
                        (set! board-posn-list (map (lambda (x) (car x)) boardl))))]
                             [(and (> x (car undoxs)) (< x (cdr undoxs)) (g:can-undo))
                              (begin
                                (every-init-undo-redo)
                                (let ([boardl (g:undo)])
                                  (displayln "undo")
                        (set! board-image-list (map (lambda (x) (cdr x)) boardl))
                        (set! board-posn-list (map (lambda (x) (car x)) boardl))))]
                             )]
                      [else (display "X:") (displayln x)
                            (display "Y:") (displayln y) (newline)]
                      )
                    ]
                  )]
       [(and (mouse=? input "move")
             (or (equal? state 'double)
                 (equal? state 'single))
             (not undo-redo-state))
        (let ([x1 (x-nearest x)]
              [y1 (y-nearest y)]
              [nx (xn-nearest x)]
              [ny (yn-nearest y)])
          (set! moving-animation-posn-list2 (list (make-posn x1 y1)))
          (if (car (g:legal nx ny move))
              (begin
              (set! moving-animation-image-list (list (if
                                                 (equal? move 'black)
                                                 black-move
                                                 white-move)))
              (set! moving-animation-posn-list (list (make-posn x1 y1))))
              (begin
              (set! moving-animation-image-list '())
              (set! moving-animation-posn-list '()))
              ))
        ]
       )))
  (on-key (lambda (nothing akey)
            (cond [(key=? akey "up") (set! rule-image (last-image rule-image))]
                  [(key=? akey "down") (set! rule-image (next-image rule-image))]
                  [(key=? akey "home")
                   (set! image-list main-menu-image-list)
                       (set! posn-list main-menu-posn-list)
                       (set! state 'main)
                       (set! rule-image (cons 0 (get-image 0)))])))
  )

