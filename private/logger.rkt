#lang racket/base

(require racket/match)

(provide start-logging
         parse-logging-level)


(define (parse-logging-level level)
  (match level
    ["debug"
     'debug]
    ["info"
     'info]
    ["warning"
     'warning]
    ["error"
     'error]
    ["fatal"
     'fatal]
    [else
     (raise-argument-error 'parse-logging-level "one of (fatal error warning info debug)" level)]))
         

(define logging-thread #f)

(define (start-logging level)
  (set! logging-thread (thread (λ ()
                                 (define lr (make-log-receiver (current-logger) level #f))
                                 (let loop ()
                                   (let ([msg (sync lr)])
                                     (eprintf "~a~n" (vector-ref msg 1)))
                                   (loop))))))


