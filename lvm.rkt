#lang racket/base


(require racket/system
         racket/port)


(provide logical-volume-exists?
         snapshot-logical-volume
         remove-logical-volume
         merge-snapshot)

;todo - pipe output to a pipe and only display it if there is an error


(define (logical-volume-exists? path)
    (system (format "lvdisplay ~a" path)))


(define (snapshot-logical-volume path snapshot-name size #:rw? (rw? #t))
    (unless (system (format "lvcreate --snapshot --size ~a  --permission ~a --name ~a ~a" size (if rw? "rw" "r") snapshot-name path))
      (error 'snapshot-logical-volume "failed to create snapshot ~v of logical volume ~v" snapshot-name path))
    (void))

(define (remove-logical-volume path)
    (unless (system (format "lvremove -f ~a" path))
      (error 'snapshot-logical-volume "failed to remove logical volume ~v" path))
    (void))
  

(define (merge-snapshot path)
    (unless (system (format "lvconvert --merge ~a" path))
      (error 'mege-snapshot "failed to merge snapshot ~v" path))
    (void))
