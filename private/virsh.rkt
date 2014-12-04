#lang racket/base


(require racket/system
         racket/port)


(provide dump-xml
         guest-running?
         shutdown-guest
         start-guest)


(define (dump-xml name)
  (let ([output (open-output-string)])
    (if (parameterize ([current-output-port output]
                           [current-error-port output])
              (system (format "virsh dumpxml ~a" name)))
      (error 'virsh "failed to dump xml for guest ~a. output: ~a" name (get-output-string output))
      (get-output-string output))))


(define (guest-running? name)
  (let ([output (open-output-string)])
    (unless (parameterize ([current-output-port output]
                           [current-error-port output])
              (system "virsh list"))
      (error 'virsh "failed to list guest operating systems with virsh. output: ~a" (get-output-string output)))
      (regexp-match? (pregexp (format "\\s~a\\s" (regexp-quote name))) (get-output-string output))))


(define (shutdown-guest name #:timeout (timeout 300) #:poll (poll 1))
  (unless (guest-running? name)
    (error 'shutdown-guest "guest ~a not running" name))
  
  (let ([output (open-output-string)])
    (unless (parameterize ([current-output-port output]
                           [current-error-port output])
              (system (format "virsh shutdown ~a" name)))
      (error 'shutdown-guest "virsh failed to shutdown guest ~a: output: ~a" name (get-output-string output))))
  
  (let ([timeout-alarm (alarm-evt (+ (current-inexact-milliseconds) (* timeout 1000)))])
    (let loop ()
      (if (equal? timeout-alarm (sync/timeout poll timeout-alarm))
          (error 'shutdown-guest "timeout ~a exceeded while waiting for guest ~a to shutdown~n" timeout name)
          (when (guest-running? name)
            ;virsh shutdown is repeated every poll times, had problems with winserver2008 domain controller 
            ;not listening to the first shutdown command
            (let ([output (open-output-string)])
              (parameterize ([current-output-port output]
                             [current-error-port output])
                (system (format "virsh shutdown ~a" name))))
            (loop))))))


(define (start-guest name)
  (let ([output (open-output-string)])
    (unless (parameterize ([current-output-port output]
                           [current-error-port output])
              (system (format "virsh start ~a" name)))
      (error 'startup-guest "virsh failed to startup guest ~a: output: ~a" name (get-output-string output)))))

  
         






          
          