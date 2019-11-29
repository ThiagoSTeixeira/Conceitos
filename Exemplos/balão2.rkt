#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)
 
(define (balloon b) (circle (car b) "solid" (cadr b)))
 
(define (blow-up b k) (list (+ (car b) 5) (list-ref (list "blue" "yellow" "green" "gold"  "black" "red") (random 6))))
 
(define (deflate b)  (list (max (- (car b) 1) 1) (cadr b)))
 
(big-bang (list 50 "blue") (on-key blow-up) (on-tick deflate)
          (to-draw balloon 500 500))