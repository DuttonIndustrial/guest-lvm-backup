#lang racket/base

(require racket/file
         racket/match
         racket/port
         racket/system)

(provide rdiff-sig-proc
         rdiff-delta-proc
         rdiff-patch-proc)


(define (rdiff-signature basis-file-i signature-file-o)
  (parameterize ([current-output-port signature-file-o]
                 [current-input-port basis-file-i])
    (system (format "rdiff signature - -"))))

(define (rdiff-delta signature-file new-file-i delta-file-o)
  (parameterize ([current-output-port delta-file-o]
                 [current-input-port new-file-i])
    (unless (system (format "rdiff delta ~a - -" signature-file))
      (error 'rdiff-delta-failed))))
  
(define (rdiff-patch basis-file delta-file new-file-o)
  (parameterize ([current-output-port new-file-o])
  (unless (system (format "rdiff patch ~a ~a -" basis-file delta-file))
      (error 'rdiff-patch-failed))))




(define (rdiff-sig-proc basis-file-i signature-o #:block-size (block-size (* 1024 1024 1024)))
  (define-logger rdiff-sig-proc)
  (let loop ([block-count 0])
    
    (define block-start-position (file-position basis-file-i))
    (define basis-block-i (make-limited-input-port basis-file-i block-size #f))
    (define sig-bytes  (with-output-to-bytes 
                            (λ ()
                              (rdiff-signature basis-block-i (current-output-port)))))
    (define block-end-position (file-position basis-file-i))
    
    (if (= block-start-position block-end-position)
        (begin
          (log-rdiff-sig-proc-info "processed ~a bytes into ~a blocks" block-end-position block-count)
          (write (list 'completed block-end-position) signature-o)
          (flush-output signature-o))
        (begin
          (write (list 'signature block-start-position block-end-position (bytes-length sig-bytes)) signature-o)
          (write-bytes sig-bytes signature-o)
          (flush-output signature-o)
          (log-rdiff-sig-proc-info "block [~a - ~a] has signature length ~a" block-start-position block-end-position (bytes-length sig-bytes))
          (loop (add1 block-count))))))
        

(define (rdiff-delta-proc signature-i delta-o src-i)
  (define-logger rdiff-delta-proc)
  (define block-signature-file (make-temporary-file "block-signature~a.bin"))
  (log-rdiff-delta-proc-info "chose ~a for signature working file" block-signature-file)
  (let loop ()
    (match (read signature-i)
      
      [(list 'completed final-position)
       (write (list 'completed final-position) delta-o)
       (flush-output delta-o)
       (log-rdiff-delta-proc-info "completed")]
       
      
      [(list 'signature block-start block-end signature-length)
       (define block-size (- block-end block-start))
       (log-rdiff-delta-proc-info "received signature for block [~a - ~a] with signature size ~a" block-start block-end signature-length)
       
       ;write the block data to a file
       (with-output-to-file block-signature-file
         #:mode 'binary
         #:exists 'truncate/replace
         (λ ()
           (copy-port (make-limited-input-port signature-i signature-length #f) (current-output-port))))

       (define delta-bytes (with-output-to-bytes 
                            (λ ()
                              (define block-i (make-limited-input-port src-i block-size #f))
                              (rdiff-delta block-signature-file block-i (current-output-port)))))

       ;rdiff delta signature-i src-i delta-o
       (write (list 'delta block-start block-end (bytes-length delta-bytes)) delta-o)
       (write-bytes delta-bytes delta-o)
       (log-rdiff-delta-proc-info "wrote delta block [~a - ~a] ~a" block-start block-end (bytes-length delta-bytes))
       (flush-output delta-o)
       (loop)]))
  (log-rdiff-delta-proc-info "removing signature working file ~a" block-signature-file)
  (delete-file block-signature-file))
       

(define (rdiff-patch-proc delta-i basis-i new-file-o)
  (define-logger rdiff-patch-proc)
  (define block-delta-file (make-temporary-file "block-delta~a.bin"))
  (define block-basis-file (make-temporary-file "block-basis~a.bin"))
  
  (log-rdiff-patch-proc-info "chose ~a for delta working file" block-delta-file)
  (log-rdiff-patch-proc-info "chose ~a for basis working file" block-basis-file)
  
  (let loop ()
    (match (read delta-i)
      [(list 'delta block-start block-end delta-length)
       (define block-size (- block-end block-start))
       (log-rdiff-patch-proc-info "processing block ~a - ~a delta-length ~a" block-start block-end delta-length)
       
       ;write the delta block data to a file
       (with-output-to-file block-delta-file
         #:mode 'binary
         #:exists 'truncate/replace
         (λ ()
           (copy-port (make-limited-input-port delta-i delta-length #f) (current-output-port))))
       
       (with-output-to-file block-basis-file
         #:mode 'binary
         #:exists 'truncate/replace
         (λ ()
           (copy-port (make-limited-input-port basis-i block-size #f) (current-output-port))))
      
       (rdiff-patch block-basis-file block-delta-file new-file-o)
       (loop)]
      
      [(list 'completed final-position)
       (log-rdiff-patch-proc-info "completed patching")]))
  
  (log-rdiff-patch-proc-info "removing delta working file ~a" block-delta-file)
  (log-rdiff-patch-proc-info "removing basis working file ~a" block-basis-file)
  (delete-file block-delta-file)
  (delete-file block-basis-file))

  
