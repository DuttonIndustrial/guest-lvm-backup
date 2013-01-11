#lang racket/base


(require racket/system
         racket/port)


(provide guest-running?
         shutdown-guest
         start-guest)




(define (guest-running? name)
  (let ([output (open-output-string)])
    (unless (parameterize ([current-output-port output]
                           [current-error-port output])
              (system "virsh list --name"))
      (error 'virsh "failed to list guest operating systems with virsh. output: ~a" (get-output-string output)))
      (regexp-match? (format "(?mi:^~a$)" (regexp-quote name)) (get-output-string output))))
  
  
(define (shutdown-guest name #:timeout (timeout 30) #:poll (poll 1))
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
          (if (guest-running? name)
              (loop)
              (void))))))


(define (start-guest name)
  (let ([output (open-output-string)])
    (unless (parameterize ([current-output-port output]
                           [current-error-port output])
              (system (format "virsh start ~a" name)))
      (error 'startup-guest "virsh failed to startup guest ~a: output: ~a" name (get-output-string output)))))

  
         





          
          