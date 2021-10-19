#lang racket

(require racket/file)


(define (parser some-file)
(define stack (list "stmt" "stmtlist"))
(define inline(file->lines some-file))
;local function llp:
  (define (llp lst len)
    (cond
     [(list-prefix? '("$$ ") lst) "Accept"]
     [else
      (define input (string-split (first lst)))
      (println input)
      (cond
        [(list-prefix? '("read") input)
         (set! stack (append(list "read" "id") stack))
         (if (< (length input) 3) (llp (rest lst) (+ len 1)) (error "Syntax error found on line " (~a len)))
         ]
        [(list-prefix? '("write") input)
         (set! stack (append(list "write" "expr") stack))
         (llp (rest lst) (+ len 1))
         ]
        [else  (llp (rest lst) (+ len 1))]
        )
      ]))



  
  ;body of parser calls llp:
  (llp inline 1))




;function call here
(parser "Input03.txt")


;(define inlist(file->list "Input01.txt"))
;(for ([x inlist])
;(println x))









