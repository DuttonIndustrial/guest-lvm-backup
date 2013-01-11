#lang racket/base

(require racket/system)


(provide ssh-command)

(define (ssh-command username host command)
  (system (format "ssh ~a@~a '~a'" username host command)))
