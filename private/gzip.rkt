#lang racket/base

(require racket/system)



(provide gzip
         gunzip)


(define (gzip input output)
  (parameterize ([current-input-port input]
                 [current-output-port output])
    (system "gzip -c")))

(define (gunzip input output)
  (parameterize ([current-input-port input]
                 [current-output-port output])
    (system "gunzip -c")))
