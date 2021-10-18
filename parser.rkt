#lang racket

(require racket/file)
(provide file->list)



;;(define in (open-input-file "Input01.txt"))
;;(read-line in)
;;(close-input-port in)

(define (parser some-file)
(define templist (list "stmt" "stmtlist"))
(define inline(file->lines some-file))

(for-each (lambda (arg)
              (printf "Got ~a\n" arg)
              23)
            inline)

(define firstitem (first inline))
(set! inline(remove firstitem inline))
(display inline)
(display "\n")

(define item(list-ref inline 0))
(define pusheditem(string-split item))
(display item)
(append pusheditem templist))



;;function call here
(parser "Input01.txt")









