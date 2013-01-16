#lang racket/base

(require racket/cmdline
         racket/file
         racket/match
         
         
         "private/control.rkt"
         "private/logger.rkt"
         "private/remote-backup.rkt")

(define log-level (make-parameter #f))

(command-line
 #:program "guest-lvm-full-backup"
 #:once-each 
 [("--log-level") ll
                 "log racket messages to standard-error port."
                 "accepts one of debug info warning error fatal"
                 (log-level (parse-logging-level ll))]
 
 
 #:args (host-name guest-name snapshot-time)
 
 ;setup logging
 (when (log-level)
   (start-logging (log-level)))

 (define-logger guest-lvm-backup-full-backup)
 (log-guest-lvm-backup-full-backup-debug "starting up")

 (let ([temp-filename (make-temporary-file "basis~a" #f (remote-basis-directory))])
   (finally 
    (begin
      (log-guest-lvm-backup-full-backup-info "transferring data to ~a" temp-filename)
      (with-output-to-file temp-filename
        #:mode 'binary
        #:exists 'truncate/replace
        (λ ()
          (let loop ()
            (match (read)
               [(list 'bytes bytes-count)
                (write-bytes (read-bytes bytes-count))
                (loop)]
               [(list 'done)
                (log-guest-lvm-backup-full-backup-info "completed transferring")]
              [else
               (error 'transfer-failed "unexpected message: ~v" else)]))
          
           (rename-file-or-directory temp-filename (build-path (remote-basis-directory) (remote-basis-filename host-name guest-name snapshot-time))))))
    (begin
      (when (file-exists? temp-filename)
        (log-guest-lvm-backup-full-backup-info "cleaning up temp file ~a" temp-filename)
        (delete-file temp-filename))))))


