#lang racket/base

(require racket/contract/base
         racket/format
         racket/list
         racket/math)

(provide (contract-out [print-progress
                        (->* ((real-in 0 1))
                             (#:barlength exact-positive-integer?
                              #:linewidth exact-positive-integer?
                              #:output output-port?) 
                             #:rest (listof any/c)
                             any/c)]
                       [copy-port-progress
                        (->* ((-> positive? exact-positive-integer? any/c) input-port?)
                             (#:report-interval positive?)
                             #:rest (non-empty-listof output-port?)
                             any/c)]
                       [make-progress-reporter
                        (-> output-port? exact-positive-integer? (-> positive? exact-positive-integer? any/c))]))

(define (print-progress percent 
                        #:barlength (barlength 20)
                        #:linewidth (linewidth 80)
                        #:output (output (current-output-port))
                        . args)
  (fprintf output
           "\r~a" 
          (~a "[" 
              (~a (~a "." 
                      #:width (round (* percent barlength))
                      #:left-pad-string "." 
                      #:align 'right)
                  #:width barlength
                  #:right-pad-string " "
                  #:align 'left)
              "] "
              (round (* percent 100))
              "% "
              (if (empty? args) "" (apply format args))
              #:width linewidth
              #:align 'left
              #:right-pad-string " "
              #:limit-marker "..."))
  (flush-output output))


(define (mb/sec total-time-ms total-bytes)
  (if (or (<= total-bytes 0) (<= total-time-ms 0))
      0
      (/ total-bytes total-time-ms 1000.0) ))


;copies data from current-input-port to current-output-port
;copy-port calls progress with the number of bytes written after each round of writing
(define (copy-port-progress progress 
                            #:report-interval (report-interval 1)
                            in 
                            . outs)
  
  (define bytes (make-bytes 4096))
  (define start-time (current-inexact-milliseconds))
  
  (let loop ([count 0]
             [next-report 0])
    (let ([amount (read-bytes-avail! bytes in)])
      (if (eof-object? amount)
          (progress start-time count)
          (begin
            (for-each (λ (out)
                        (write-bytes bytes out 0 amount))
                      outs)
            (if (> (current-inexact-milliseconds) next-report)
              (begin
                (progress start-time (+ count amount))
                (loop (+ count amount) (+ (current-inexact-milliseconds) (* report-interval 1000))))
              (loop (+ count amount) next-report)))))))
            
        
        
(define (make-progress-reporter progress-port total-count)
  (λ (start-time count)
    (print-progress (/ count total-count)
                    #:output progress-port
                    "~a of ~a bytes at ~a MB/s"
                    count
                    total-count
                    (format-rational (mb/sec (- (current-inexact-milliseconds) start-time)
                                             count) 2))
    
    (when (= total-count count)
      (fprintf progress-port "~n"))))


(define (format-rational num precision)
  (/ (round (* num (expt 10 precision))) (expt 10 precision) 1.0))
  
 