#lang racket



;(make-test-files 1000000 1 "test-src" "test-dst")
;(files-equal? "test-src" "test-dst")
  
(define (test)
  
  (define source-name "test-src")
  (define dest-name "test-dst")
  
  (printf "making test files~n")
  (make-test-files 10000000 10000 "test-src" "test-dst")
  
  
  (printf "beginning test~n")
  (define dest-file-i (open-input-file dest-name #:mode 'binary))
  (define dest-file-o (open-output-file dest-name #:mode 'binary #:exists 'update))
  
  (define src-file-i (open-input-file source-name #:mode 'binary))
  
  (define-values (dest-i src-o) (make-pipe))
  (define-values (src-i dest-o) (make-pipe))
  
  (define src (thread (λ () 
                        (copy-proc-src dest-i dest-o src-file-i #:block-size 5000)
                        (close-input-port dest-i)
                        (close-output-port dest-o)
                        (close-input-port src-file-i))))
  
  (define dest (thread (λ () 
                         (copy-proc-dest src-i src-o dest-file-i dest-file-o)
                         (close-input-port src-i)
                         (close-output-port src-o)
                         (close-input-port dest-file-i)
                         (close-output-port dest-file-o))))
  
  (thread-wait src)
  (thread-wait dest)
  
  (printf "completed test~n")
  
  (files-equal? source-name dest-name))
