#lang racket/base

(require racket/system)


(provide ssh-command)

;the command is executed remotely
;and the current thread waits until it completes
;if a break is raised on the thread executing system, the break will be passed
;to the program on the other side of the terminal.
;this program will then wait for its exit.
;all input is sent via current-input-port, current-output-port, current-error-port
(define (ssh-command username host command #:port (port #f) #:identity (identity #f))
  (let ([commandline (format "ssh -t ~a@~a ~a ~a '~a'" 
                          username 
                          host 
                          (if port (format "-p ~a" port) "")
                          (if identity (format "-i ~a" identity) "")
                          command)])
    (unless (system commandline)
      (error 'ssh-command "ssh command failed ~a" commandline))
    (void)))
