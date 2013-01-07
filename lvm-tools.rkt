#lang racket/base


(require racket/system
         racket/port)


(provide logical-volume-exists?
         snapshot-logical-volume
         remove-logical-volume)


(define (logical-volume-exists? path)
  (parameterize ([current-output-port (open-output-nowhere)])
    (system (format "lvdisplay ~v" path))))


(define (snapshot-logical-volume path snapshot-name size)
  (unless (system (format "lvcreate --snapshot --size ~a --name ~v ~v" size snapshot-name path))
    (error 'snapshot-logical-volume "failed to create snapshot ~v of logical volume ~v" snapshot-name path))
  (void))

(define (remove-logical-volume path)
  (unless (system (format "lvremove -f ~v" path))
    (error 'snapshot-logical-volume "failed to remove logical volume ~v" path))
  (void))
  

