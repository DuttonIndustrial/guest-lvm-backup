#lang racket/base


(require racket/system
         setup/getinfo)

(provide installer)


(define (installer racket-install-dir collection-dir)
  (define info (get-info/full collection-dir))
  (define link-destination (info 'launcher-link-destination))
           
  (for-each (λ (launcher)
              (let ([to (build-path link-destination launcher)]
                    [path (simplify-path (build-path racket-install-dir "bin" launcher))])
                
                (printf "~a link ~a -> ~a~n" (if (link-exists? to)
                                                 (begin
                                                   (delete-file to)
                                                   "updating")
                                                 "creating")
                        to path)
                
                (make-file-or-directory-link path to)))
                
            (info 'racket-launcher-names)))
                       
              
              