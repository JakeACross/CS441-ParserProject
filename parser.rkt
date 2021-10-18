#lang racket

(require racket/file)


(define (parser some-file)
(define templist (list "stmt" "stmtlist"))
(define inline(file->lines some-file))
;local function llp:
  (define (llp lst len)
    (cond
     [(list-prefix? '("$$ ") lst) "Accept"]
     [else
      (println len)

      (llp (rest lst) (+ len 1))





           ]))



  
  ;body of parser calls llp:
  (llp inline 0))




;function call here
(parser "Input01.txt")










