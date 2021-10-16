#lang racket

(require racket/file)
(provide file->list)



;;(define in (open-input-file "Input01.txt"))
;;(read-line in)
;;(close-input-port in)

(define inline(file->lines "Input01.txt"))

(for-each (lambda (arg)
              (printf "Got ~a\n" arg)
              23)
            inline)







