#lang racket/base

(require racket/cmdline
         
         "private/control.rkt"
         "private/logger.rkt"
         "private/gzip.rkt"
         "private/rdiff.rkt"
         "private/remote-backup.rkt")


(define log-level (make-parameter #f))
(define block-size (make-parameter (* 512 1024 1024)))

(command-line
 #:program "guest-lvm-backup"
 #:once-each 
 [("--log-level") ll
                 "log racket messages to standard-error port."
                 "accepts one of debug info warning error fatal"
                 (log-level (parse-logging-level ll))]
 
 [("--block-size") bs
                   "set transfer block size in bytes"
                   "the default is 512Mb"
                   (block-size (string->number bs))]
 
 #:args (basis-file)
 
 ;setup logging
 (when (log-level)
   (start-logging (log-level)))

 (define-logger guest-lvm-backup-signature)
 (log-guest-lvm-backup-signature-info "starting up")
 (pipeline (λ ()
             (with-input-from-file (build-path (remote-basis-directory) basis-file)
               #:mode 'binary
               (λ ()
                 (gunzip (current-input-port) (current-output-port)))))
           (λ ()
             (rdiff-sig-proc (current-input-port) (current-output-port) (block-size)))))
          


   