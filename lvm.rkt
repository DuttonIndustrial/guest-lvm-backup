#lang racket/base


(require racket/list
         racket/port
         racket/system)


(provide logical-volume-exists?
         snapshot-logical-volume
         remove-logical-volume
         logical-volume-size)

;todo - pipe output to a pipe and only display it if there is an error


(define (logical-volume-exists? path)
  (define output (open-output-string))
  (unless (parameterize ([current-output-port output]
                         [current-error-port output])
            (system (format "lvdisplay ~a" path)))
    (error 'logical-volume-exists? "failed to determine if volume ~a exists. error:" (get-output-string output))))


(define (snapshot-logical-volume path snapshot-name size #:rw? (rw? #t))
  (define output (open-output-string))
  (unless (parameterize ([current-output-port output]
                         [current-error-port output])
            (system (format "lvcreate --snapshot --size ~a  --permission ~a --name ~a ~a" size (if rw? "rw" "r") snapshot-name path)))
    (error 'snapshot-logical-volume "failed to create snapshot ~v of logical volume ~a. error ~a" snapshot-name path (get-output-string output)))
  (void))

(define (remove-logical-volume path)
  (define output (open-output-string))
  (unless (parameterize ([current-output-port output]
                         [current-error-port output])
            (system (format "lvremove -f ~a" path)))
    (error 'snapshot-logical-volume "failed to remove logical volume ~a. error ~a" path (get-output-string output)))
  (void))

(define (logical-volume-size path)
  (define output (open-output-string))
  (unless (parameterize ([current-output-port output])
            (system (format "lvs --noheading --units b --nosuffix -o lv_size  ~a" path)))
    (error 'logical-volume-size "command lvs failed. error ~a" (get-output-string output)))
  (string->number (first (regexp-match #rx"[0-9]+" (get-output-string output)))))