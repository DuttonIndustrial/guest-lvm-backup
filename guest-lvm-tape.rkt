#lang racket/base

(require racket/cmdline
         racket/list
         racket/path
         racket/port
         mzlib/os
         net/sendmail
         
         
         "private/control.rkt"
         "private/gzip.rkt"
         "private/logger.rkt"
         "private/lvm.rkt"
         "private/mt.rkt"
         "private/progress.rkt"
         "private/time.rkt"
         "private/virsh.rkt")

(define std-out (current-output-port))

(define-logger guest-lvm-tape)

(define guest-name (make-parameter #f))

(define log-level (make-parameter #f))

(define email-to (make-parameter empty))

(define eject (make-parameter #f))

(command-line
 #:program "guest-lvm-tape"
          
 #:once-each 
 ["--eject"
   "Eject the tape after completion"
   (eject #t)]

 [("-g") guest
         "Shutdown/restart guest prior to snapshotting"
         (guest-name guest)]
 
 [("--log-level") ll
                  "log racket messages to standard-error port."
                  "accepts one of debug info warning error fatal"
                  (log-level (parse-logging-level ll))]
 
 #:multi
 ["--email-to" to
        "Send email to user upon completion"
        (email-to (cons to (email-to)))]
 
 #:args (lvm-disk-path device)
 
  (log-guest-lvm-tape-info "~a: starting up" (now))
  
  (define snapshot-time (current-seconds))
  
  (define snapshot-name (format "~a-backup-~a" (file-name-from-path lvm-disk-path)  snapshot-time))

  (define snapshot-logical-path (apply build-path (append (explode-path (path-only lvm-disk-path)) (list snapshot-name))))
  
  
 (call-with-exception-handler 
  (λ (exn)
    (unless (empty? (email-to))
      (send-mail-message #f
                         (format "Tape backup failed: ~a" (exn-message exn))
                         (email-to)
                         empty
                         empty
                         (list "Attempted tape backup failed. A tape needs to be loaded"
                               (format "Host: ~a" (gethostname))
                               (format "Device: ~a" device)
                               (when (guest-name)
                                 (format "Guest: ~a" (guest-name)))
                               (format "Volume: ~a" lvm-disk-path))
                         #f))
    exn)
  (λ ()
  (unless (logical-volume-exists? lvm-disk-path)
    (error 'no-volume "logical volume ~v does not exist." lvm-disk-path))
 
  (define volume-size (logical-volume-size lvm-disk-path))
  (log-guest-lvm-tape-info "~a: volume size is ~a bytes" (now) volume-size)
    
  (unless (mt-online? device)
    (error 'no-tape "tape ~a doesn't appear to be online" device))
  
  (if (guest-name)
      (begin
        (printf "~a: shutting down guest ~a~n" (now) guest-name)
        (shutdown-guest guest-name)
  
        (finally (begin
                   (printf "~a: snapshotting ~a as ~a~n" (now) lvm-disk-path snapshot-logical-path)
                   (snapshot-logical-volume lvm-disk-path snapshot-name "10G"))
                 (begin
                   (printf "~a: starting guest ~a~n" (now) guest-name)
                   (start-guest guest-name))))
      (begin
        (printf "~a: snapshotting ~a as ~a~n" (now) lvm-disk-path snapshot-logical-path)
        (snapshot-logical-volume lvm-disk-path snapshot-name "10G")))
  
  (finally
   (begin
     (log-guest-lvm-tape-info "~a: rewinding" (now) device)
     (mt-rewind device)
   
     ;write snapshot name and time to first tape file
     (log-guest-lvm-tape-info "~a: writing snapshot information" (now) device)
     (with-output-to-file device
       #:mode 'binary
       #:exists 'update
       (λ ()
         (when (guest-name)
           (printf "guest: ~a~n" (guest-name)))
         
         (printf "size: ~a~n" volume-size)
         
         (printf "volume: ~a~n" lvm-disk-path)
         
         (printf "time: ~a~n" snapshot-time)))
     
     ;write snapshot gzip file to second tape device
     (log-guest-lvm-tape-info "~a: writing snapshot volume to ~a" (now) device)
     (pipeline
      (λ ()
        (with-input-from-file snapshot-logical-path
                               #:mode 'binary
                               (λ ()
                                 (copy-port-progress (make-progress-reporter std-out volume-size) (current-input-port) (current-output-port)))))
      (λ ()
        (with-output-to-file device
          #:mode 'binary
          #:exists 'update
          (λ ()
            (gzip (current-input-port) (current-output-port))))))
     
     
     (log-guest-lvm-tape-info "~a: weof device ~a" (now) device)
     (mt-weof device)
   
     
     (log-guest-lvm-tape-info "~a: rewinding device ~a" (now) device)
     (mt-rewind device)
   
     (when (eject)
       (log-guest-lvm-tape-info "~a: ejecting device" (now) device)
       (mt-offline device))
   
     (unless (empty? (email-to))
       (send-mail-message #f
                         "Tape backup completed."
                         (email-to)
                         empty
                         empty
                         (list "Tape backup succeeded"
                               (when (eject)
                                 "A new tape needs to be loaded.")
                               (format "Host: ~a" (gethostname))
                               (when (guest-name)
                                 (format "Guest: ~a" (guest-name)))
                               (format "Volume: ~a" lvm-disk-path)
                               (format "Device: ~a" device)
                               (format "Size: ~a" volume-size)
                               (format "Snapshot time: ~a" snapshot-time))
                         #f)))
    (begin
     (log-guest-lvm-tape-info "~a: removing logical volume ~a" (now) snapshot-logical-path)
     (remove-logical-volume snapshot-logical-path))))))
  
  
  
  
 
 
 
 
 ;shutdown guest
 
 ;snapshot lvm
 
 ;startup guest
 
 ;pump file with progress to gzip to tape
 
 ;eject the tape 
 
 ;send email upon completion
 
           
 
