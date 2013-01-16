#lang setup/infotab



(define name "guest-lvm-backup")
(define compile-omit-paths '("test"))
(define racket-launcher-names (list "guest-lvm-backup"  "guest-lvm-tape"))
(define racket-launcher-libraries (list "guest-lvm-backup.rkt" "guest-lvm-tape.rkt"))

