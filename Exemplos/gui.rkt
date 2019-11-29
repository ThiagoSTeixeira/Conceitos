#lang racket/gui

(define secret (random 5))
 
(define f (new frame% [label "Guessing game"])) ; toplevel window
(define t (new message% [parent f]
               [label "Can you guess the number I'm thinking about?"]))
(define p (new horizontal-pane% [parent f]))    ; horizontal container
 
(define ((make-check i) btn evt)
  (message-box "." (cond [(< i secret) "Too small"]
                         [(> i secret) "Too big"]
                         [else         "Exactly!"]))
  (when (= i secret) (send f show #f)))         ; success => close window
 
(for ([i (in-range 10)])                        ; create all buttons
  (make-object button% (format "~a" i) p (make-check i)))
 
(send f show #t) ; show the window to start the application
