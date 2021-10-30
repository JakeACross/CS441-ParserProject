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
      ;local function recursion:
      (define (recursion item)
        (println stack)
        (println item)
        (cond
            [(list-prefix? '("id") stack)
             (cond
               [(string->number (first item))
                (error "Syntax error found on line" len)]
               [else
                (set! stack (rest stack))
                (recursion (rest item))])]

            [(list-prefix? '(":=") stack)
             (cond
               [(list-prefix? '(":=") item)
                (set! stack (rest stack))
                (recursion (rest item))
                ]
               [else
                (error "Syntax error found on line" len)])]
        
            [(list-prefix? '("expr") stack)
             (set! stack (rest stack))
             (set! stack (append(list "term" "term_tail") stack))
             (recursion item)
             ]
            
        
          [(list-prefix? '("term") stack)
           (set! stack (rest stack))
           (set! stack (append(list "factor" "factor_tail") stack))
           (recursion item)
           ]
          
        
          [(list-prefix? '("factor") stack)
           (set! stack (rest stack))
           (cond
               [(string->number (first item))
                (recursion (rest item))]
               [else
                (cond
                  [(string-prefix? (first item) "(")
                   (for ([i (first item)])
                     (if (eq? i #\()
                          (set! stack (append(list "factor_tail" "term_tail" ")") stack))
                          (recursion (rest item))))]
                  [(string-contains? (first item) ")")
                   (for ([i (first item)])
                     (cond [(eq? i #\))
                          (if (eq? ")" (third stack))
                              (set! stack (list-tail stack 3))
                              (error "Syntax error found on line" len))]))
                   (recursion (rest item))]
                  [(list-prefix? '("+") item)
                   (error "Syntax error found on line" len)]
                  [(list-prefix? '("-") item)
                   (error "Syntax error found on line" len)]
                  [(list-prefix? '("*") item)
                   (error "Syntax error found on line" len)]
                  [(list-prefix? '("/") item)
                   (error "Syntax error found on line" len)]
                  
                  [else
                   (recursion (rest item))])])]
          
          [(list-prefix? '("factor_tail") stack)
           (set! stack (rest stack))
           (cond
             [(empty? item)]
               [(string->number (first item))
                (error "Syntax error found on line" len)]
               [else
                (cond
                  [(list-prefix? '("*") item)
                   (set! stack (append(list "factor" "factor_tail") stack))
                   (recursion (rest item))
                   ]
                  [(list-prefix? '("/") item)
                   (set! stack (append(list "factor" "factor_tail") stack))
                   (recursion (rest item))
                   ]
                  [(list-prefix? '("+") item)
                   (recursion item)
                   ]
                  [(list-prefix? '("-") item)
                   (recursion item)
                   ]
                  [else
                   (error "Syntax error found on line" len)])])]
          
          [(list-prefix? '("term_tail") stack)
           (set! stack (rest stack))
           (cond
             [(empty? item)]
               [(string->number (first item))
                (error "Syntax error found on line" len)]
               [else
                (cond
                  [(list-prefix? '("+") item)
                   (set! stack (append(list "term" "term_tail") stack))
                   (recursion (rest item))
                   ]
                  [(list-prefix? '("-") item)
                   (set! stack (append(list "term" "term_tail") stack))
                   (recursion (rest item))
                   ]
                  [else
                   (error "Syntax error found on line" len)])])]

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
         (llp (rest lst) (+ len 1))
         ]
        [else  
         (set! stack (append(list ":=" "expr") stack))
         (recursion (rest input))
         (llp (rest lst) (+ len 1))]
        )
      ]
     )
    )
 
  ;body of parser calls llp:
  (llp inline 1))




;function call here
(parser "ParserInputFiles/Input06.txt")



