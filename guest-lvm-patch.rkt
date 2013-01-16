#lang racket/base

(require racket/async-channel
         racket/cmdline
         racket/file
         racket/match
         racket/port
         
         openssl/sha1
         
         "private/control.rkt"
         "private/gzip.rkt"
         "private/logger.rkt"
         "private/progress.rkt"
         "private/rdiff.rkt"
         "private/remote-backup.rkt"
         "private/time.rkt")

(define log-level (make-parameter #f))
(define progress? (make-parameter #f))

(command-line
 #:program "guest-lvm-patch"
 #:once-each 
 [("--log-level") ll
                 "log racket messages to standard-error port."
                 "accepts one of debug info warning error fatal"
                 (log-level (parse-logging-level ll))]
 
 [("--progress") "Show patch progress"
                 (progress? #t)]
 
 #:args (basis-file host-name guest-name snapshot-time)
 
 ;setup logging
 (when (log-level)
   (start-logging (log-level)))
   
  
  (define stdout (current-output-port))
  (define-logger guest-lvm-backup-patch)
  (log-guest-lvm-backup-patch-info "~a: starting up" (now))
  
  (unless (file-exists? (build-path (remote-basis-directory) basis-file))
    (error 'no-basis-file "basis files ~a not found" basis-file))
  
  (define temp-filename (make-temporary-file "backup~a" #f (remote-basis-directory)))
  (log-guest-lvm-backup-patch-info "using ~a as temporary working file" temp-filename)
  
  (finally
   (begin
     
     (define volume-size (match (read)
                           [(list 'volume-size volume-size)
                            volume-size]
                           [else
                            (error 'message-not-understood "failed to understand message ~v" else)]))
     
     (log-guest-lvm-backup-patch-info "received volume size of ~a" volume-size)
     
     (define backup-sha1sum-channel (make-async-channel))
     
     (with-output-to-file temp-filename
       #:mode 'binary
       #:exists 'truncate/replace
       (λ ()    
         (let*-values ([(gunzipped-i gunzipped-o) (make-pipe (* 1024 1024))])
           (watch (λ ()
                    (with-input-from-file (build-path (remote-basis-directory) basis-file)
                      #:mode 'binary
                      (λ ()
                        (log-guest-lvm-backup-patch-info "unzipping ~a" (build-path (remote-basis-directory) basis-file))
                        (gunzip (current-input-port) gunzipped-o)
                        (close-output-port gunzipped-o))))
                  (λ ()
                    (pipeline
                     (λ ()
                       (log-guest-lvm-backup-patch-info "starting rdiff patch proc")
                       (rdiff-patch-proc (current-input-port) gunzipped-i (current-output-port)))
                     (λ ()
                       (tee (λ ()
                              (let ([backup-sha1sum (sha1-bytes (current-input-port))])
                                (log-guest-lvm-backup-patch-info "backup sha1sum is ~v" backup-sha1sum)
                                (async-channel-put backup-sha1sum-channel backup-sha1sum)))))
                     (λ ()
                       (if (progress?)
                           (copy-port-progress (make-progress-reporter stdout volume-size) (current-input-port) (current-output-port))
                           (copy-port (current-input-port) (current-output-port))))
                     (λ ()
                       (gzip (current-input-port) (current-output-port)))))))))
     
     (log-guest-lvm-backup-patch-info "comparing sha1sum from snapshot and backup file")
     
     (match (read)
       [(list 'snapshot-sha1sum snapshot-sha1sum)
        (log-guest-lvm-backup-patch-info "received snapshot sha1sum of ~v" snapshot-sha1sum)
        (unless (equal? snapshot-sha1sum (async-channel-get backup-sha1sum-channel))
          (error 'sha1sum-not-matching "sha1sum of original and backup to not match"))]
       [else
        (error 'message-not-understoon "didn't understand received message ~a" else)])
     
     (rename-file-or-directory temp-filename (build-path (remote-basis-directory) (remote-basis-filename host-name guest-name snapshot-time))))
   (when (file-exists? temp-filename)
     (log-guest-lvm-backup-patch-info "removing ~a" temp-filename)
     (delete-file temp-filename))))


