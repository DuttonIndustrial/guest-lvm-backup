#lang setup/infotab



(define name "guest-lvm-backup")
(define compile-omit-paths '("test"))
(define racket-launcher-names (list "guest-lvm-backup"  "guest-lvm-full-backup" "guest-lvm-locate" "guest-lvm-patch" "guest-lvm-signature" "guest-lvm-tape"))
(define racket-launcher-libraries (list "guest-lvm-backup.rkt"  "guest-lvm-full-backup.rkt" "guest-lvm-locate.rkt" "guest-lvm-patch.rkt" "guest-lvm-signature.rkt" "guest-lvm-tape.rkt"))
(define install-collection "private/installer.rkt")
(define post-install-cleanup
(define launcher-link-destination "/usr/local/bin")
