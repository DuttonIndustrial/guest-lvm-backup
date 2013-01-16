#lang racket/base

(require racket/list)

(provide remote-basis-regexp
         locate-basis-file
         remote-basis-filename
         remote-basis-directory)

(define (remote-basis-regexp hostname guestname)
  (regexp (format "(?mi:^~a-~a-backup-([0-9]+).gz$)" (regexp-quote hostname) (regexp-quote guestname))))


#|finds the latest file to use as a basis file|#
(define (locate-basis-file hostname guestname)
  (let ([basis-times (filter-map (λ (x)
                                   (cond [(regexp-match (remote-basis-regexp hostname guestname) x)
                                          => (compose string->number second)]
                                         [else
                                          #f]))
                                 (map path->string (directory-list (remote-basis-directory))))])
    (if (empty? basis-times)
        #f
        (remote-basis-filename hostname guestname (apply max basis-times)))))



(define (remote-basis-filename hostname guestname seconds)
  (format "~a-~a-backup-~a.gz" hostname guestname seconds))


(define (remote-basis-directory)
  "backups")

