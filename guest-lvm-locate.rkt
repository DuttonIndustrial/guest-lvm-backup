#lang racket/base

(require racket/cmdline
         racket/list
         "private/logger.rkt"
         "private/remote-backup.rkt")

(define log-level (make-parameter #f))


(command-line
 #:program "guest-lvm-locate"
 #:once-each 
 [("--log-level") ll
                 ("log racket messages to standard-error port."
                 "accepts one of debug info warning error fatal")
                 (log-level (parse-logging-level ll))]
 
 #:args (hostname guestname)
 
 ;setup logging
 (when (log-level)
   (start-logging (log-level)))

  (define-logger guest-lvm-backup-basis-locate)
  (log-guest-lvm-backup-basis-locate-info "starting up")
  (let ([basis-file (locate-basis-file hostname guestname)])
    (if basis-file
        (begin
          (log-guest-lvm-backup-basis-locate-info "found basis file ~a" basis-file)
          (write (list 'basis-file basis-file)))
        (begin
          (log-guest-lvm-backup-basis-locate-info "no basis file found.")
          (write (list 'no-basis-file)))))
  (flush-output))


