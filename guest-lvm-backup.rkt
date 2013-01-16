#lang racket/base

(require mzlib/os
         openssl/sha1
         racket/async-channel
         racket/cmdline
         racket/file
         racket/function
         racket/list
         racket/match
         racket/path
         racket/port
         racket/system
         
         "private/control.rkt"
         "private/gzip.rkt"
         "private/logger.rkt"
         "private/lvm.rkt"
         "private/progress.rkt"
         "private/rdiff.rkt"
         "private/remote-backup.rkt"
         "private/ssh.rkt"
         "private/time.rkt"
         "private/virsh.rkt")



(define ssh-port (make-parameter #f))
(define ssh-identity (make-parameter #f))
(define ssh-user (make-parameter #f))
(define log-level (make-parameter #f))
(define shutdown-guest? (make-parameter #t))
(define progress? (make-parameter #f))
(define snapshot-size (make-parameter "5G"))
(define block-size (make-parameter (* 512 1024 1024)))

(command-line
 #:program "guest-lvm-backup"
 #:once-each 
 [("--ssh-identity") identity 
                     "Use ssh identity file"
                     (ssh-identity identity)]
 
 [("--ssh-port") port
                 "Use ssh port"
                 (ssh-port port)]
 
 [("--ssh-user") user
                 "Use ssh user"
                 (ssh-user user)]
 
 [("--no-shutdown-guest") guest
              "Do not shutdown guest prior to snapshot creation"
              "You can also specify this if the guest is already shutdown"
              (shutdown-guest #f)]
 
 [("--snapshot-size") ss
              "Size of lvm device snapshot."
              "Default is 5G"
              (snapshot-size ss)]
 
 [("--log-level") ll
                 "log racket messages to standard-error port."
                 "accepts one of debug info warning error fatal"
                 (log-level (parse-logging-level ll))]
 
 [("--block-size") bs
                   "set transfer block size in bytes"
                   "the default is 512Mb"
                   (block-size (string->number bs))]
 
 [("--progress") "Show backup progress"
                 (progress? #t)]
 
 #:args (guest-name lvm-disk-path remote-host)
 
 ;setup logging
 (when (log-level)
   (start-logging (log-level)))
    
 (define-logger guest-lvm-backup)
 
 (define std-out (current-output-port))
 
 (log-guest-lvm-backup-info "~a: starting up" (now))
 
 (define snapshot-time (current-seconds))
 
 (define snapshot-name (format "~a-backup-~a" (file-name-from-path lvm-disk-path)  snapshot-time))
 
 (define snapshot-logical-path (apply build-path (append (explode-path (path-only lvm-disk-path)) (list snapshot-name))))
 
 
 (unless (logical-volume-exists? lvm-disk-path)
   (error 'no-volume "logical volume ~v does not exist." lvm-disk-path))
 
 (if (shutdown-guest)
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
       (snapshot-logical-volume lvm-disk-path snapshot-name (snapshot-size))))
       
 
 (define volume-size (logical-volume-size lvm-disk-path))
 (log-guest-lvm-backup-info "~a: volume size is ~a bytes" (now) volume-size)
 
 (finally 
  (pipeline (λ ()
              (ssh-command remote-host 
                           (format "guest-lvm-locate ~a ~a ~a" 
                                   (if (log-level) 
                                       (format "--log-level ~a" (log-level))
                                       "")
                                   (gethostname)
                                   guest-name)
                           #:user (ssh-user)
                           #:port (ssh-port)
                           #:identity (ssh-identity))
              (log-guest-lvm-backup-info "~a: locate process completed" (now)))
            (λ ()
              (match (read)
                [(list 'no-basis-file)     
                 (printf "~a: performing full backup~n" (now))
                 (pipeline (λ ()
                             (with-input-from-file snapshot-logical-path
                               #:mode 'binary
                               (λ ()
                                 (if (progress?)
                                      (copy-port-progress (make-progress-reporter std-out volume-size) (current-input-port) (current-output-port))
                                      (copy-port (current-input-port) (current-output-port))))))
                           
                           (λ ()
                             (gzip (current-input-port) (current-output-port)))
                           
                           (λ ()
                             (define bytes (make-bytes (* 64 1024)))
                             (let loop ()
                               (let ([count (read-bytes-avail! bytes)])
                                 (unless (eof-object? count)
                                   (write (list 'bytes count))
                                   (write-bytes bytes (current-output-port) 0 count)
                                   (loop))))
                             
                             (write (list 'done)))
                           
                           (λ ()
                             (ssh-command  remote-host 
                                          (format "guest-lvm-full-backup ~a ~a ~a ~a" 
                                                  (if (log-level) 
                                                      (format "--log-level ~a" (log-level))
                                                      "")
                                                  (gethostname) 
                                                  guest-name 
                                                  snapshot-time)
                                          #:user (ssh-user)
                                          #:port (ssh-port)
                                          #:identity (ssh-identity))))
                 
                 (printf "~a: completed full backup~n" (now))]
                
                
                [(list 'basis-file basis-file)
                 
                 (printf "~a: performing differential backup~n" (now))
                 
                 (define snapshot-sha1sum-channel (make-async-channel))
                 
                 (pipeline (λ ()
                             (ssh-command remote-host 
                                          (format "guest-lvm-signature ~a ~a ~a"
                                                  (if (log-level) 
                                                      (format "--log-level ~a" (log-level))
                                                      "")
                                                  basis-file
                                                  (block-size))
                                          #:user (ssh-user)
                                          #:port (ssh-port)
                                          #:identity (ssh-identity)))
                           (λ ()
                             (define signature-in (current-input-port))
                             
                             (with-input-from-file snapshot-logical-path
                               #:mode 'binary
                               (λ ()
                                 (pipeline (λ ()
                                             (tee (λ ()
                                                    (let ([sha1sum (sha1-bytes (current-input-port))])
                                                      (log-guest-lvm-backup-info "sha1sum of snapshot is ~v" sha1sum)
                                                      (async-channel-put snapshot-sha1sum-channel sha1sum)))))
                                           (λ ()
                                             (write (list 'volume-size volume-size))
                                             (rdiff-delta-proc signature-in (current-output-port) (current-input-port))
                                             (write (list 'snapshot-sha1sum (async-channel-get snapshot-sha1sum-channel))))))))
                           
                           (λ ()
                             (ssh-command remote-host 
                                          (format "guest-lvm-patch ~a ~a ~a ~a ~a ~a"
                                                  (if (log-level) 
                                                      (format "--log-level ~a" (log-level))
                                                      "")
                                                  (if (progress?)
                                                      "--progress"
                                                      "")
                                                  basis-file 
                                                  (gethostname) 
                                                  guest-name 
                                                  snapshot-time)
                                          #:user (ssh-user)
                                          #:port (ssh-port)
                                          #:identity (ssh-identity))))
                 
                 (printf "~a: completed differential backup~n" (now))]
                
                [else
                 (error 'unknown-message "~v" else)])))
  (begin
    (log-guest-lvm-backup-info "~a: removing logical volume ~a" (now) snapshot-logical-path)
    (remove-logical-volume snapshot-logical-path))))






  
  