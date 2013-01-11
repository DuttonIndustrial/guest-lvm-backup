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
         "gzip.rkt"
         "lvm.rkt"
         "rdiff.rkt"
         "ssh.rkt"
         "virsh.rkt")


(define (remote-basis-filename hostname guestname seconds)
  (format "~a-~a-backup-~a.gz" hostname guestname seconds))

(define (remote-basis-regexp hostname guestname)
  (regexp (format "(?mi:^~a-~a-backup-([0-9]+).gz$)" (regexp-quote hostname) (regexp-quote guestname))))


#|finds the latest file to use as a basis file|#
(define (locate-basis-file hostname guestname)
  (let ([basis-times (filter-map (λ (x)
                                   (cond [(regexp-match (remote-basis-regexp hostname guestname) x)
                                          => (compose string->number second)]
                                         [else
                                          #f]))
                                 (map path->string (directory-list)))])
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
          (let*-values ([(unzipped-basis-i unzipped-o) (make-pipe (* 1024 1024))]
                        [(gunzip-thread) (thread (λ () 
                                                   (with-input-from-file basis-file
                                                     #:mode 'binary
                                                     (λ ()
                                                       (gunzip (current-input-port) unzipped-o)
                                                       (close-output-port unzipped-o)))))]
                        
                        [(signature-thread) (thread (λ ()
                                                      (rdiff-sig-proc unzipped-basis-i (current-output-port))))])
                        
                        
            (thread-wait gunzip-thread)
            (thread-wait signature-thread)))
        (begin
          (log-guest-lvm-backup-basis-info "no basis file found. Initiating full transfer")
          (write (list 'no-basis-file))
          (flush-output)
          
          (match (read)
            [(list 'basis-file snapshot-seconds)
             ;TODO: temp-file needs removed if an error occurs
             (define temp-filename (make-temporary-file "basis~a"))
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
                     [eof-object
                      (error 'transfer-failed "failed to transfer basis file.")]))))
             (rename-file-or-directory temp-filename (remote-basis-filename hostname guestname snapshot-seconds))]
            [else
             (error 'unkown-message else)])))))
          


(define (guest-lvm-backup-patch-proc basis-file hostname guestname snapshot-time)
  (define-logger guest-lvm-backup-patch)
  (log-guest-lvm-backup-patch-info "starting up")
  
  (unless (file-exists? basis-file)
    (error 'no-basis-file "basis files ~a not found" basis-file))
  
  ;TODO: this needs removed if an error occurs
  (define temp-filename (make-temporary-file "backup~a"))
  
  ;startup an rdiff patch process
  (let*-values ([(gunzipped-i gunzipped-o) (make-pipe (* 1024 1024))]
                [(gunzip-thread) (thread (λ ()
                                           (with-input-from-file basis-file
                                             #:mode 'binary
                                             (λ ()
                                               (gunzip (current-input-port) gunzipped-o)
                                               (close-output-port gunzipped-o)))))]
                [(gzipped-i gzipped-o) (make-pipe (* 1024 1024))]
                [(gzip-thread) (thread (λ ()
                                        (with-output-to-file temp-filename
                                          #:mode 'binary
                                          #:exists 'truncate/replace
                                          (λ ()
                                            (gzip gzipped-i (current-output-port))))))])
    (rdiff-patch-proc (current-input-port) gunzipped-i gzipped-o)
    (close-output-port gzipped-o)
    (thread-wait gunzip-thread)
    (thread-wait gzip-thread)
    (rename-file-or-directory temp-filename (remote-basis-filename hostname guestname snapshot-time))))



#|
 guest-lvm-backup guestname lvmdiskpath remoteuser remotehost

launches racket guest-lvm-backup.rkt basis hostname guestname on remote machine - it starts up

shutdowns the guest
takes a snapshot
restarts the guest
copies up the difference to remote host with remote user using the remotebasis_name.
 

remotebasisname files are actually (format "~a_(secondssnapshottime).gz")
|#
(define (guest-lvm-backup guestname lvmdiskpath remoteuser remotehost #:snapshot-size (snapshot-size 10))
  (define-logger guest-lvm-backup)
  (log-guest-lvm-backup-info "starting up")
  
  (define snapshot-time (current-seconds))
  
  (define snapshot-name (format "~a-backup-~a" (file-name-from-path lvmdiskpath)  snapshot-time))

  (define snapshot-logical-path (apply build-path (append (explode-path (path-only lvmdiskpath)) (list snapshot-name))))

 
  (unless (logical-volume-exists? lvmdiskpath)
    (error 'no-volume "logical volume ~v does not exist." lvmdiskpath))
  
  (log-guest-lvm-backup-info "shutting down guest ~a" guestname)
  (shutdown-guest guestname)
  
  (log-guest-lvm-backup-info "snapshotting ~a as ~a" lvmdiskpath snapshot-logical-path)
  (snapshot-logical-volume lvmdiskpath snapshot-name "10G")
  
  (log-guest-lvm-backup-info "starting guest ~a" guestname)
  (start-guest guestname)
  
  (define snapshot-size (logical-volume-size snapshot-logical-path))
  (log-guest-lvm-backup-info "snapshot size is ~a bytes" snapshot-size)
  
  
  ;start remote basis process
  (define-values (basis-i to-basis) (make-pipe (* 1024 1024)))
  (define-values (from-basis basis-o) (make-pipe (* 1024 1024)))
  
  (log-guest-lvm-backup-info "launching basis program against ~a@~a" remoteuser remotehost)
  
  (define basis-thread (thread (λ ()
                               (parameterize ([current-output-port basis-o]
                                              [current-input-port basis-i])
                                 (ssh-command remoteuser remotehost (format "racket -W info lvmsync/guest-lvm-backup.rkt basis ~a ~a" (gethostname) guestname)))
                                 (log-guest-lvm-backup-info "basis process completed"))))
  
  
  (match (read from-basis)
    [(list 'no-basis-file)
     (write (list 'basis-file snapshot-time) to-basis)
     
     (let*-values ([(snapshot-i snapshot-o) (make-pipe (* 1024 1024))]
                   [(gzipped-i gzipped-o) (make-pipe (* 1024 1024))]
                   [(snapshot-thread) (thread (λ ()
                                              (with-input-from-file snapshot-logical-path
                                                #:mode 'binary
                                                (λ ()
                                                  (copy-port (current-input-port) snapshot-o)
                                                  (close-output-port snapshot-o)))))]
                   
                   [(gzip-thread) (thread (λ ()
                                            (parameterize ([current-input-port snapshot-i]
                                                           [current-output-port gzipped-o])
                                              (system "gzip -c"))
                                            (close-output-port gzipped-o)))])
       (let loop ()
         (let ([bytes (read-bytes (* 1024 1024) gzipped-i)])
           (unless (eof-object? bytes)
             (write (list 'bytes (bytes-length bytes)) to-basis)
             (write-bytes bytes to-basis)
             (loop))))
       
       (write (list 'done) to-basis)
       (flush-output to-basis)
       (thread-wait basis-thread)
       (log-guest-lvm-backup-info "completed transfer of basis"))]
    
    
    [(list 'basis-file basis-file)
     (log-guest-lvm-backup-info "using ~a as basis file for signature" basis-file)
     
     ;start up a delta process and pipe signature to delta
     (let*-values ([(delta-i delta-o) (make-pipe (* 1024 1024))]
                   [(patch-i patch-o) (make-pipe (* 1024 1024))]
                   [(patch-thread) (thread (λ ()
                                             (parameterize ([current-output-port patch-o]
                                                            [current-input-port delta-i])
                                               (ssh-command remoteuser remotehost (format "racket -W info lvmsync/guest-lvm-backup.rkt patch ~a ~a ~a ~a" basis-file (gethostname) guestname snapshot-time)))
                                             (log-guest-lvm-backup-info "patch process completed")))]
                   [(delta-thread) (thread (λ ()
                                            (with-input-from-file snapshot-logical-path
                                              #:mode 'binary
                                              (λ ()
                                                (rdiff-delta-proc from-basis delta-o (current-input-port))
                                                (close-output-port delta-o)))
                                             (log-guest-lvm-backup-info "delta process completed")))])
       
                   
       (thread-wait patch-thread)
       (thread-wait delta-thread)
       (thread-wait basis-thread)
       (log-guest-lvm-backup-info "completed backup!"))]
                                              
    [else
     (error 'unknown-message "~a" else)])
  
  (log-guest-lvm-backup-info "removing logical volume ~a" snapshot-logical-path)
  (remove-logical-volume snapshot-logical-path))
     
     

(match (current-command-line-arguments)
  [(vector "basis" hostname guestname)
   (guest-lvm-backup-basis-proc hostname guestname)]
  
  [(vector "patch" basis-file hostname guestname snapshot-time)
  (guest-lvm-backup-patch-proc basis-file hostname guestname snapshot-time)]
  
  [(vector guestname lvmdiskpath remoteuser remotehost)
   (guest-lvm-backup guestname lvmdiskpath remoteuser remotehost)]
  
  [else
   (error 'bad-arguments "commandline not recognized ~a" (current-command-line-arguments))])
   
  
  
  
  
  
  
  
  
  