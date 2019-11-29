#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)
 
(define (balloon b) (circle b "solid" "red"))
 
(define (blow-up b k) (+ b 5))
 
(define (deflate b) (max (- b 1) 1))
 
(big-bang 50 (on-key blow-up) (on-tick deflate)
          (to-draw balloon 200 200))