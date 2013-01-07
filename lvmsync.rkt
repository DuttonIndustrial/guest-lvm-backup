#lang racket/base

(require openssl/sha1
         racket/match)


(define (ssh-lvm-backup local-volume remote-user remote-host remote-port remote-volume)
  ;snapshot source drive
  ;snapshot dest drive

  ;for each block in source-snapshot
	;checksum block 
	;checksum block in dest
	;if checksums differ, copy the block
	
;compute running checksum on the entire process as you progress on both sides
;if the checksum is the same on both sides we are done
  #f
  )




(define (sha1-file filename)
  (define input (open-input-file filename #:mode 'binary))
  (begin0 
    (sha1-bytes input)
    (close-input-port input)))



(define (copy-proc-dest src-i src-o dest-file-i dest-file-o)
  (define block-size (match (read src-i)
                       [(list 'block-size block-size)
                        block-size]
                       [else
                        (error 'copy-proc-dest "failed to retreive block size")]))
  (write 'ok src-o)
  (log-debug "copy-proc-dest: using block-size ~v" block-size)
  
  (let loop ([block 0])
    (let* ([bytes (read-bytes block-size dest-file-i)])
      
      (write (list 'block-checksum block (if (eof-object? bytes)
                                             0
                                             (sha1-bytes (open-input-bytes bytes)))) src-o)
            
      (match (read src-i)
        ['completed
         (log-debug "copy-proc-dest: completed!")]
        
        [(list 'block-bytes remote-block remote-bytes)
         (unless (equal? remote-block block)
           (error 'out-of-sync "received remote block ~v. Expected block ~v" remote-block block))
         (file-position dest-file-o (* block-size block))
         (write-bytes remote-bytes dest-file-o)
         (loop (add1 block))]
        
        [(list 'skip-block block)
         (loop (add1 block))]))))


(define (copy-proc-src dest-i dest-o src-file-i #:block-size (block-size 1024))
  (write (list 'block-size block-size) dest-o)
  (unless (equal? (read dest-i) 'ok)
    (error 'copy-proc-src "block size transmit at start failed"))
  
  (let loop ([block 0]
             [copy-count 0])
    (let ([bytes (read-bytes block-size src-file-i)])
      
      (if (eof-object? bytes)
          (begin
            (write 'completed dest-o)
            (log-debug "copy-proc-src: completed - copied ~v blocks!" copy-count))
          (begin
            (match (read dest-i)
              [(list 'block-checksum dest-block dest-block-checksum)
               (unless (equal? dest-block block)
                 (error 'out-of-sync "copy-proc-src: received checksum from destination block ~v. Expected ~v" dest-block block))
               (if (equal? dest-block-checksum (sha1-bytes (open-input-bytes bytes)))
                   (begin
                     (write (list 'skip-block block) dest-o)
                     (loop (add1 block) copy-count))
                   (begin
                     (write (list 'block-bytes block bytes) dest-o)
                     (loop (add1 block) (add1 copy-count))))]
                            
              [else
               (error 'unexpected-message else)]))))))
          
  
  
  
(define (make-test-files size dest-modifcation-count src-file-name dest-file-name)
  (with-output-to-file src-file-name #:mode 'binary #:exists 'truncate/replace
    (λ ()
      (let loop ([count 0])
        (unless (equal? count size)
          (write-byte (random 256))
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

(define (files-equal? file1 file2)
    (equal? (sha1-file file1) (sha1-file file2)))


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



  