#lang racket/base

(require racket/cmdline
         racket/date
         racket/path
         "lvm-tools.rkt")

(define dest-logical-path (command-line
                   #:program "lvmsync-dest"
                   #:args (dest-file)
                   dest-file))
  

(date-display-format 'iso-8601)

(define snapshot-name (format "~a-lvmsync-~a" (file-name-from-path dest-logical-path)  (current-seconds)))

(define snapshot-logical-path (apply build-path (append (explode-path (path-only dest-logical-path)) (list snapshot-name))))


(printf "~v~n~v~n" snapshot-name snapshot-logical-path)

;check for lv file
(unless (logical-volume-exists? dest-logical-path)
  (error 'no-volume "logical volume ~v does not exist." dest-logical-path))

;create snapshot of logical volume


(snapshot-logical-volume dest-logical-path snapshot-name)
  
  


