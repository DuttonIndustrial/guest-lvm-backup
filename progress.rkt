#lang racket/base

(require racket/format
         racket/list
         racket/math)

(provide print-progress
         copy-port-progress)

(define (print-progress percent 
                        #:barlength (barlength 20)
                        #:linewidth (linewidth 80)
                        #:output (output (current-output-port))
                        . args)
  (fprintf output
           "\r~a" 
          (~a "[" 
              (~a (~a "." 
                      #:width (exact-round (* percent barlength))
                      #:left-pad-string "." 
                      #:align 'right)
                  #:width barlength
                  #:right-pad-string " "
                  #:align 'left)
              "] "
              (~r (* percent 100)
                  #:min-width 4
                  #:precision 1)
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
;reports the progress to progress-output-port every progress-time-interval

(define (copy-port-progress progress-output-port progress-time total buffer-size)
  
  (define start-time (current-inexact-milliseconds))
  
  (let loop ([next-progress 0]
             [count 0])
    (if (> (current-inexact-milliseconds) next-progress)
        (begin
          (print-progress (/ count total)
                              #:output progress-output-port
                              "~a of ~a bytes at ~a MB/s"
                              count
                              total
                              (~r (mb/sec (- (current-inexact-milliseconds) start-time)
                                      count)
                                  #:precision 2))
          (loop (+ (current-inexact-milliseconds) progress-time) count))
        (let ([bytes (read-bytes buffer-size)])
          (if (eof-object? bytes)
              (begin
                (print-progress 1
                                #:output progress-output-port
                                "~a of ~a bytes at ~a MB/s"
                                count
                                total
                                (~r (mb/sec (- (current-inexact-milliseconds) start-time)
                                        count)
                                    #:precision 2))
                (fprintf progress-output-port "~n")
                (close-output-port (current-output-port)))
              (begin
                (write-bytes bytes)
                (loop next-progress (+ count (bytes-length bytes)))))))))
  
  