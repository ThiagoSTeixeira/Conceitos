#lang racket
(define (move daqui pralá) (display (string-append daqui "->" pralá "\n")))

(define (torres n sai>  >chega <extra>)
  (unless (zero? n)
      (begin  
       (torres (- n 1) sai> <extra> >chega)
       (move sai> >chega) 
       (torres (- n 1) <extra> sai> >chega))))