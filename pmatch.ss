(library (pmatch)
  (export pmatch)
  (import
   (chezscheme))

  (define-syntax ppat
    (syntax-rules (quote unquote)
      [(_ v uscore kt kf)
       (and (identifier? #'uscore) (free-identifier=? #'uscore #'_)) 
       kt]
      [(_ v () kt kf)
       (if (null? v)
           kt
           kf)]
      [(_ v (quote lit) kt kf)
       (if (equal? v (quote lit))
           kt
           kf)]
      [(_ v (unquote var) kt kf)
       (let ([var v])
         kt)]
      [(_ v (x . y) kt kf)
       (if (pair? v)
           (let ([vx (car v)]
                 [vy (cdr v)])
             (ppat vx x (ppat vy y kt kf) kf))
           kf)]
      [(_ v lit kt kf)
       (if (equal? v (quote lit))
           kt
           kf)]))

  (define-syntax pmatch
    (syntax-rules (else guard)
      [(_ (rator rand ...) cs ...)
       (let ([v (rator rand ...)])
         (pmatch v cs ...))]
      [(_ v)
       (error 'pmatch "failed: ~s" v)]
      [(_ v (else e ...))
       (begin e ...)]
      [(_ v (pat (guard g ...) e ...) cs ...)
       (let ([fk (lambda () (pmatch v cs ...))])
         (ppat v pat
               (if (and g ...) (begin e ...) (fk))
               (fk)))]
      [(_ v (pat e ...) cs ...)
       (let ([fk (lambda () (pmatch v cs ...))])
         (ppat v pat (begin e ...) (fk)))])))

