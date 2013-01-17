#lang racket/base


(require racket/system
         setup/getinfo)

(provide post-installer)


(define (post-installer racket-install-dir collection-dir)
  (define info (get-info/full collection-dir))
  (define link-destination (info 'launcher-link-destination))
           
  (for-each (λ (launcher)
              (let ([target-path (build-path link-destination launcher)])
                (when (link-exists? target-path)
                  (printf "removing old link  ~a~n" target-path)
                  (delete-file target-path))
                  
                (printf "creating link ~a~n" target-path)
                (make-file-or-directory-link (build-path collection-dir "bin" launcher) target-path)))
            (info 'racket-launcher-names)))
                       
              
              