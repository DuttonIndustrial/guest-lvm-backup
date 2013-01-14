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
         "control.rkt"
         "gzip.rkt"
         "lvm.rkt"
         "progress.rkt"
         "rdiff.rkt"
         "ssh.rkt"
         "time.rkt"
         "virsh.rkt")


(define (remote-basis-filename hostname guestname seconds)
  (format "~a-~a-backup-~a.gz" hostname guestname seconds))

(define (remote-basis-regexp hostname guestname)
  (regexp (format "(?mi:^~a-~a-backup-([0-9]+).gz$)" (regexp-quote hostname) (regexp-quote guestname))))


(define (remote-basis-directory)
  "backups")

#|finds the latest file to use as a basis file|#
(define (locate-basis-file hostname guestname)
  (let ([basis-times (filter-map (λ (x)
                                   (cond [(regexp-match (remote-basis-regexp hostname guestname) x)
                                          => (compose string->number second)]
                                         [else
                                          #f]))
                                 (map path->string (directory-list (remote-basis-directory))))])
    (if (empty? basis-times)
        #f
        (remote-basis-filename hostname guestname (apply max basis-times)))))


(define (guest-lvm-backup-basis-locate-proc hostname guestname)
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


(define (guest-lvm-backup-full-backup-proc hostname guestname snapshot-time)
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
           
           (rename-file-or-directory temp-filename (build-path (remote-basis-directory) (remote-basis-filename hostname guestname snapshot-time))))))
     (begin
       (when (file-exists? temp-filename)
         (log-guest-lvm-backup-full-backup-info "cleaning up temp file ~a" temp-filename)
         (delete-file temp-filename))))))
  
  
        
  

;find latest basis file
;start unzipping it and sending back the signature of the file
(define (guest-lvm-backup-signature-proc basis-file)
  (define-logger guest-lvm-backup-signature)
  (log-guest-lvm-backup-signature-info "starting up")
  (pipeline (λ ()
              (with-input-from-file (build-path (remote-basis-directory) basis-file)
                #:mode 'binary
                (λ ()
                  (gunzip (current-input-port) (current-output-port)))))
            (λ ()
              (rdiff-sig-proc (current-input-port) (current-output-port) #:block-size (* 512 1024 1024)))))
          


(define (guest-lvm-backup-patch-proc basis-file hostname guestname snapshot-time)
  
  (define stdout (current-output-port))
  (define-logger guest-lvm-backup-patch)
  (log-guest-lvm-backup-patch-info "~a: starting up" (now))
  
  (unless (file-exists? (build-path (remote-basis-directory) basis-file))
    (error 'no-basis-file "basis files ~a not found" basis-file))
  
  (define temp-filename (make-temporary-file "backup~a" #f (remote-basis-directory)))
  (log-guest-lvm-backup-patch-info "using ~a as temporary working file" temp-filename)
  
  (finally
   (begin
     
     (define snapshot-size (match (read)
                             [(list 'snapshot-size snapshot-size)
                              snapshot-size]
                             [else
                              (error 'message-not-understood "failed to understand message ~v" else)]))
     
     (log-guest-lvm-backup-patch-info "received snapshot size of ~a" snapshot-size)
     
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
                       (copy-port-progress (make-progress-reporter stdout snapshot-size) (current-input-port) (current-output-port)))
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
     
     (rename-file-or-directory temp-filename (build-path (remote-basis-directory) (remote-basis-filename hostname guestname snapshot-time))))
   (when (file-exists? temp-filename)
     (log-guest-lvm-backup-patch-info "removing ~a" temp-filename)
     (delete-file temp-filename))))



#|
 guest-lvm-backup guestname lvmdiskpath remoteuser remotehost

launches racket guest-lvm-backup.rkt basis hostname guestname on remote machine - it starts up

shutdowns the guest
takes a snapshot
restarts the guest
copies up the difference to remote host with remote user using the remotebasis_name.
 

remotebasisname files are actually (format "~a_(secondssnapshottime).gz")
|#
(define (guest-lvm-backup guestname lvmdiskpath remoteuser remotehost #:snapshot-size (snapshot-size 10) #:identity (identity #f) #:port (port #f))
  (define-logger guest-lvm-backup)
  
  (define std-out (current-output-port))
  
  (log-guest-lvm-backup-info "~a: starting up" (now))
  
  (define snapshot-time (current-seconds))
  
  (define snapshot-name (format "~a-backup-~a" (file-name-from-path lvmdiskpath)  snapshot-time))

  (define snapshot-logical-path (apply build-path (append (explode-path (path-only lvmdiskpath)) (list snapshot-name))))

 
  (unless (logical-volume-exists? lvmdiskpath)
    (error 'no-volume "logical volume ~v does not exist." lvmdiskpath))
  
  (printf "~a: shutting down guest ~a~n" (now) guestname)
  (shutdown-guest guestname)
  
  (finally (begin
            (printf "~a: snapshotting ~a as ~a~n" (now) lvmdiskpath snapshot-logical-path)
            (snapshot-logical-volume lvmdiskpath snapshot-name "10G"))
           (begin
             (printf "~a: starting guest ~a~n" (now) guestname)
             (start-guest guestname)))
  
  (define snapshot-size (logical-volume-size snapshot-logical-path))
  (log-guest-lvm-backup-info "~a: snapshot size is ~a bytes" (now) snapshot-size)
  
  (finally 
     (pipeline (λ ()
                 (ssh-command remoteuser 
                              remotehost 
                              (format "racket -W info guest-lvm-backup/guest-lvm-backup.rkt locate ~a ~a" (gethostname) guestname)
                              #:port port
                              #:identity identity)
                 (log-guest-lvm-backup-info "~a: locate process completed" (now)))
               (λ ()
                 (match (read)
                   [(list 'no-basis-file)     
                    (pipeline (λ ()
                                (with-input-from-file snapshot-logical-path
                                  #:mode 'binary
                                  (λ ()
                                    (copy-port-progress (make-progress-reporter std-out snapshot-size) (current-input-port) (current-output-port)))))
                              
                              (λ ()
                                (gzip (current-input-port) (current-output-port)))
                              
                              (λ ()
                                (let loop ()
                                  (let ([bytes (read-bytes (* 1024 1024))])
                                    (unless (eof-object? bytes)
                                      (write (list 'bytes (bytes-length bytes)))
                                      (write-bytes bytes)
                                      (loop))))
                                
                                (write (list 'done)))
                              
                              (λ ()
                                (ssh-command remoteuser 
                                             remotehost 
                                             (format "racket -W info guest-lvm-backup/guest-lvm-backup.rkt full-backup ~a ~a ~a" (gethostname) guestname snapshot-time)
                                             #:port port
                                             #:identity identity)))
                    
                    (log-guest-lvm-backup-info "~a: completed full transfer of basis" (now))]
                   
                   [(list 'basis-file basis-file)
                    
                    (define snapshot-sha1sum-channel (make-async-channel))
                    
                    (pipeline (λ ()
                                (ssh-command remoteuser 
                                             remotehost 
                                             (format "racket -W info guest-lvm-backup/guest-lvm-backup.rkt signature ~a " basis-file)
                                             #:port port
                                             #:identity identity))
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
                                                (write (list 'snapshot-size snapshot-size))
                                                (rdiff-delta-proc signature-in (current-output-port) (current-input-port))
                                                (write (list 'snapshot-sha1sum (async-channel-get snapshot-sha1sum-channel))))))))
                              
                              (λ ()
                                (ssh-command remoteuser 
                                             remotehost 
                                             (format "racket -W info guest-lvm-backup/guest-lvm-backup.rkt patch ~a ~a ~a ~a" basis-file (gethostname) guestname snapshot-time)
                                             #:port port
                                             #:identity identity)))
                    
                    
                    (log-guest-lvm-backup-info "completed backup!")]
                   
                   [else
                    (error 'unknown-message "~v" else)])))
   (begin
     (log-guest-lvm-backup-info "~a: removing logical volume ~a" (now) snapshot-logical-path)
     (remove-logical-volume snapshot-logical-path))))
     

(define ssh-port (make-parameter #f))
(define ssh-identity (make-parameter #f))

(command-line
 #:program "guest-lvm-backup"
 #:once-each 
 [("-i" "--identity") identity 
                      "Use ssh identity file"
                      (ssh-identity identity)]
 [("-p" "--port") port
                  "Use ssh port"
                  (ssh-port port)]
 #:args args
 (match args
   [(list "locate" hostname guestname)
    (guest-lvm-backup-basis-locate-proc hostname guestname)]
   
   [(list "full-backup" hostname guestname snapshot-time)
    (guest-lvm-backup-full-backup-proc hostname guestname snapshot-time)]
    
   [(list "signature" basis-file)
    (guest-lvm-backup-signature-proc basis-file)]
   
   [(list "patch" basis-file hostname guestname snapshot-time)
    (guest-lvm-backup-patch-proc basis-file hostname guestname snapshot-time)]
   
   [(list guestname lvmdiskpath remoteuser remotehost)
    (guest-lvm-backup guestname lvmdiskpath remoteuser remotehost #:port (ssh-port) #:identity (ssh-identity))]
   
   [else
    (error 'bad-arguments "commandline not recognized ~a" else)]))
  
  
  
  
  
  
  
  
  