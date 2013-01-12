#lang racket/base

(require mzlib/os
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



;find latest basis file
;start unzipping it and sending back the signature of the file
(define (guest-lvm-backup-basis-proc hostname guestname)
  (define-logger guest-lvm-backup-basis)
  (log-guest-lvm-backup-basis-info "starting up")
  (let ([basis-file (locate-basis-file hostname guestname)])
    (if basis-file
        (begin
          (log-guest-lvm-backup-basis-info "found basis file ~a" basis-file)
          (write (list 'basis-file basis-file))
          (flush-output)
          (pipeline (λ ()
                      (with-input-from-file (build-path (remote-basis-directory) basis-file)
                        #:mode 'binary
                        (λ ()
                          (gunzip (current-input-port) (current-output-port)))))
                    (λ ()
                      (rdiff-sig-proc (current-input-port) (current-output-port) #:block-size (* 512 1024 1024)))))
        (begin
          (log-guest-lvm-backup-basis-info "no basis file found. Initiating full transfer")
          (write (list 'no-basis-file))
          (flush-output)
          
          (let ([temp-filename (make-temporary-file "basis~a" #f (remote-basis-directory))])
            (finally 
             (match (read)
               [(list 'basis-file snapshot-seconds)
                ;TODO: temp-file needs removed if an error occurs
                (log-guest-lvm-backup-basis-info "transferring data to ~a" temp-filename)
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
                         (log-guest-lvm-backup-basis-info "completed transferring")]
                        [else
                         (error 'transfer-failed "failed to transfer basis file... unexpected message: ~a" else)]))))
                (rename-file-or-directory temp-filename (build-path (remote-basis-directory) (remote-basis-filename hostname guestname snapshot-seconds)))]
               [else
                (error 'unkown-message else)])
             (begin
               (when (file-exists? temp-filename)
                 (log-guest-lvm-backup-basis-info "cleaning up temp file ~a" temp-filename)
                 (delete-file temp-filename)))))))))
          


(define (guest-lvm-backup-patch-proc basis-file hostname guestname snapshot-time)
  (define-logger guest-lvm-backup-patch)
  (log-guest-lvm-backup-patch-info "~a: starting up" (now))
  
  (unless (file-exists? (build-path (remote-basis-directory) basis-file))
    (error 'no-basis-file "basis files ~a not found" basis-file))
  
  (define temp-filename (make-temporary-file "backup~a" #f (remote-basis-directory)))
  (log-guest-lvm-backup-patch-info "using ~a as temporary working file" temp-filename)
  
  (finally
   (begin
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
                       (gzip (current-input-port) (current-output-port)))))))))
     
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
  
  (log-guest-lvm-backup-info "~a: shutting down guest ~a" (now) guestname)
  (shutdown-guest guestname)
  
  (finally (begin
            (log-guest-lvm-backup-info "~a: snapshotting ~a as ~a" (now) lvmdiskpath snapshot-logical-path)
            (snapshot-logical-volume lvmdiskpath snapshot-name "10G"))
           (begin
             (log-guest-lvm-backup-info "~a: starting guest ~a" (now) guestname)
             (start-guest guestname)))
  
  (define snapshot-size (logical-volume-size snapshot-logical-path))
  (log-guest-lvm-backup-info "~a: snapshot size is ~a bytes" (now) snapshot-size)
  
  (finally 
   (begin                      
     ;start remote basis process
     (define-values (basis-i to-basis) (make-pipe (* 1024 1024)))
     (define-values (from-basis basis-o) (make-pipe (* 1024 1024)))
     
     (log-guest-lvm-backup-info "~a: launching basis program against ~a@~a" (now) remoteuser remotehost)
     
     (watch (λ ()
              (parameterize ([current-output-port basis-o]
                             [current-input-port basis-i])
                (ssh-command remoteuser 
                             remotehost 
                             (format "racket -W info guest-lvm-backup/guest-lvm-backup.rkt basis ~a ~a" (gethostname) guestname)
                             #:port port
                             #:identity identity))
              (log-guest-lvm-backup-info "~a: basis process completed" (now)))
            
            (λ ()
              (match (read from-basis)
                [(list 'no-basis-file)
                 (write (list 'basis-file snapshot-time) to-basis)
                 (flush-output to-basis)
                 
                 (pipeline (λ ()
                             (with-input-from-file snapshot-logical-path
                               #:mode 'binary
                               (λ ()
                                 (copy-port-progress std-out 1000 snapshot-size (* 64 1024)))))
                           (λ ()
                             (gzip (current-input-port) (current-output-port)))
                           
                           (λ ()
                             (let loop ()
                               (let ([bytes (read-bytes (* 1024 1024) (current-input-port))])
                                 (unless (eof-object? bytes)
                                   (write (list 'bytes (bytes-length bytes)) to-basis)
                                   (write-bytes bytes to-basis)
                                   (loop))))
                             
                             (write (list 'done) to-basis)
                             (flush-output to-basis)
                             (log-guest-lvm-backup-info "~a: completed transfer of basis" (now))))]
                
                
                [(list 'basis-file basis-file)
                 (log-guest-lvm-backup-info "~a: using ~a as basis file for signature" (now) basis-file)
                 
                 
                 (with-input-from-file snapshot-logical-path
                   #:mode 'binary
                   (λ ()
                     (pipeline (λ ()
                                 (rdiff-delta-proc from-basis (current-output-port) (current-input-port))
                                 (log-guest-lvm-backup-info "delta process completed"))
                               (λ ()
                                 (ssh-command remoteuser 
                                              remotehost 
                                              (format "racket -W info guest-lvm-backup/guest-lvm-backup.rkt patch ~a ~a ~a ~a" basis-file (gethostname) guestname snapshot-time)
                                              #:port port
                                              #:identity identity)
                                 (log-guest-lvm-backup-info "patch process completed")))))
                 
                 (log-guest-lvm-backup-info "completed backup!")]
                
                [else
                 (error 'unknown-message "~a" else)]))))
   (begin
     (log-guest-lvm-backup-info "~a: removing logical volume ~a" (now) snapshot-logical-path)
     (remove-logical-volume snapshot-logical-path))))
     

(match (current-command-line-arguments)
  [(vector "basis" hostname guestname)
   (guest-lvm-backup-basis-proc hostname guestname)]
  
  [(vector "patch" basis-file hostname guestname snapshot-time)
  (guest-lvm-backup-patch-proc basis-file hostname guestname snapshot-time)]
  
  [(vector guestname lvmdiskpath remoteuser remotehost port identity)
   (guest-lvm-backup guestname lvmdiskpath remoteuser remotehost #:port port #:identity identity)]
  
  [else
   (error 'bad-arguments "commandline not recognized ~a" (current-command-line-arguments))])
   
  
  
  
  
  
  
  
  
  