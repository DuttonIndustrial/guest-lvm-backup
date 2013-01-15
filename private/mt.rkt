#lang racket/base


(require racket/system)


(provide mt-online?
         mt-rewind
         mt-weof
         mt-offline)


(define (mt-online? device)
  (let ([output (open-output-string)])
    (unless (parameterize ([current-output-port output]
                           [current-error-port output])
              (system (format "mt -f ~a status" device)))
      (error 'mt-online "mt failed with output: ~a" (get-output-string output)))
    (regexp-match? (pregexp (format "\\sONLINE\\s")) (get-output-string output))))


(define (mt-rewind device)
  (let ([output (open-output-string)])
    (unless (parameterize ([current-output-port output]
                           [current-error-port output])
              (system (format "mt -f ~a rewind" device)))
      (error 'mt "failed to rewind device ~a. output: ~a" device (get-output-string output)))))
      


(define (mt-weof device)
  (let ([output (open-output-string)])
    (unless (parameterize ([current-output-port output]
                           [current-error-port output])
              (system (format "mt -f ~a weof" device)))
      (error 'mt "failed to weof device  ~a. output: ~a" device (get-output-string output)))))



(define (mt-offline device)
  (let ([output (open-output-string)])
    (unless (parameterize ([current-output-port output]
                           [current-error-port output])
              (system (format "mt -f ~a offline" device)))
      (error 'mt "failed to offline device ~a. output: ~a" device (get-output-string output)))))

