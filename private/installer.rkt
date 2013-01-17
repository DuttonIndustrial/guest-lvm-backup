#lang racket/base


(require racket/system
         setup/getinfo)

(provide installer)


(define (installer racket-install-dir collection-dir)
  (define info (get-info/full collection-dir))
  (define link-destination (info 'launcher-link-destination))
           
  (for-each (λ (launcher)
              (let ([target-path (build-path racket-install-dir "bin" launcher)]
                    [destination-path (build-path link-destination launcher)])
                
                (printf "~a link ~a -> ~a~n" (if (link-exists? destination-path)
                                                 (begin
                                                   (delete-file target-path)
                                                   "updating")
                                                 "creating")
                        destination-path target-path)
                
                (make-file-or-directory-link destination-path target-path)))
                
            (info 'racket-launcher-names)))
                       
              
              