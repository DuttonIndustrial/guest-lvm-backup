#lang racket/base


(require openssl/sha1)

(provide make-test-files)

(define (make-test-files size dest-modifcation-count src-file-name dest-file-name)
  (with-output-to-file src-file-name #:mode 'binary #:exists 'truncate/replace
    (λ ()
      (let loop ([count 0])
        (unless (equal? count size)
          (let ([bytes (make-bytes (random (- size count) (random 256)))])
          (write-bytes bytes)
          (loop (add1 count))))))
  
  
  (copy-file src-file-name dest-file-name #t)
  
  (call-with-output-file #:mode 'binary #:exists 'update
    dest-file-name
    (λ (output-port)
      (let loop ([count 0])
        (unless (equal? count dest-modifcation-count)
          (file-position output-port (random size))
          (write-byte (random 256) output-port)
          (loop (add1 count))))))
  (void))
