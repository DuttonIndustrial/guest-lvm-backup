#lang racket/base


(require racket/async-channel
         racket/function
         racket/list)


(provide finally
         watch
         pipeline)

(require (for-syntax racket/base
                     syntax/parse))


(define-syntax (finally stx)
  (syntax-parse stx
                [(_ body:expr finalizer:expr)
                 #`(dynamic-wind (λ () (void))
                                 (λ ()
                                   body)
                                 (λ ()
                                   finalizer))]))


(define (watch . thunks)
  (let* ([control-channel (make-async-channel)]
         [threads (map (curry watch-thread control-channel) thunks)])
    (call-with-exception-handler 
     (λ (exn)
       (break-then-kill threads)
       exn)
     (λ ()
       (let loop ([threads threads])
         (unless (empty? threads)
           (let ([dead-thread (apply sync threads)]
                 [exn (async-channel-try-get control-channel)])
             (if exn
                 (begin
                   (break-then-kill threads)
                   (raise exn))
                 (loop (remove dead-thread threads))))))
       (void)))))


(define (watch-thread control-channel thunk)
  (parameterize ([uncaught-exception-handler (λ (exn)
                                               (async-channel-put control-channel exn)
                                               ((error-escape-handler)))])
    (thread thunk)))

;takes a list of threads and call break on each of them
;waits for up to timeout milliseconds for all threads to exit
;it forces them to be killed if any of them dont return
;and waits for all threads to complete
(define (break-then-kill threads (timeout 5000))
  (let ([kill-timeout (alarm-evt (+ (current-inexact-milliseconds) timeout))])
    (for-each break-thread threads)
    (let loop ([threads threads])
      (unless (empty? threads)
        (let ([evt (apply sync (list* kill-timeout threads))])
          (if (thread? evt)
              (loop (remove evt threads))
              (begin
                (map kill-thread threads)
                (map thread-wait threads))))))))


;each argument is a thunk to be executed in a thread
;the current-input-port and current-output-port are chained
;such that each input is chained to the previous threads output
;once the chaining happens, all threads are watched
(define (pipeline . thunks)
  (let ([final-output-port (current-output-port)])
    (apply watch
           (let loop ([thunks thunks]
                      [in (current-input-port)])
             (if (empty? (cdr thunks))
                 (list (λ ()
                         (parameterize ([current-input-port in]
                                        [current-output-port final-output-port])
                           ((car thunks)))))
                 (let-values ([(next-i o) (make-pipe (* 1024 64))])
                   (cons (λ ()
                           (parameterize ([current-input-port in]
                                          [current-output-port o])
                             (finally ((car thunks))
                                      (begin
                                        (close-output-port o)))))
                         (loop (rest thunks)
                               next-i))))))))
