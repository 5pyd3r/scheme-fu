(define-syntax λ
  (lambda (x)
    (syntax-case x ()
      [(_ args ...)
       #'(lambda args ...)])))
