#lang racket/base

(require racket/date
         racket/format
         racket/math)


(provide now
         milliseconds->string)

(date-display-format 'iso-8601)

(define (now)
  (date->string (seconds->date (current-seconds)) #t))


(define (milliseconds->string ms)
  (let*-values ([(ms) (exact-round ms)]
                [(hours hours-part) (quotient/remainder ms 3600000)]
                [(minutes minutes-part) (quotient/remainder hours-part 60000)]
                [(seconds milliseconds) (quotient/remainder minutes-part 1000)])
    (format "~a:~a:~a.~a" 
            hours 
            (~a minutes #:width 2 #:left-pad-string "0" #:align 'right)
            (~a seconds  #:width 2 #:left-pad-string "0" #:align 'right)
            (~a milliseconds #:width 3 #:left-pad-string "0" #:align 'right))))

;HH:MM:SS.MS