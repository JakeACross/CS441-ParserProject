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
      (define (recursion lst)
           (cond
            [(list-prefix? '("id") stack)
             (cond
               [(string->number (first lst))
                (error "Syntax error found on line" len)]
               [else
                (set! stack (rest stack))
               
                ]
               )
             ]
            )
           (cond
            [(list-prefix? '("expr") stack)
             (set! stack (rest stack))
             (set! stack (append(list "term" "term_tail") stack))
             ]
            )
           )
      
      (cond
        [(list-prefix? '("read") input)
         (set! stack (append(list "id") stack))
         (recursion (rest input))
         (llp (rest lst) (+ len 1))
         ]
        [(list-prefix? '("write") input)
         (set! stack (append(list "expr") stack))
         (recursion (rest input))
         ;(println stack):
         (llp (rest lst) (+ len 1))
         ]
        [else  
         (set! stack (append(list ":=" "expr") stack))
         (recursion (rest input))
         (llp (rest lst) (+ len 1))]
        )
      ]))



  
  ;body of parser calls llp:
  (llp inline 1))




;function call here
(parser "Input03.txt")



;(define inlist(file->list "Input01.txt"))
;(for ([x inlist])
;(println x))









