(import
 (cKanren mk)
 (cKanren ck)
 (cKanren fd)
 (cKanren neq)
 (cKanren tree-unify))

(define (mk-vars n)
  (cond
    ((eq? n 0) '())
    (else (cons (var n) (mk-vars (- n 1))))))

(define everyg
  (lambda (g coll)
    (cond
      ((null? coll) succeed)
      (else
       (fresh ()
         (g (car coll))
         (everyg g (cdr coll)))))))

(define split
  (lambda (coll n)
    (if (null? coll)
      coll
      (cons
       (list-head coll n)
       (split (list-tail coll n) n)))))

(define sublist
  (lambda (coll head tail)
    (list-tail (list-head coll tail) head)))

(define get-square
  (lambda (rows pos size)
    (let ([x (car pos)]
          [y (cadr pos)])
      (let loop ([n 0])
        (if (equal? n size)
          '()
          (append
           (sublist (list-ref rows (+ y n)) x (+ x size))
           (loop (+ n 1))))))))

(define inito
  (lambda (vars hints)
    (if (or (null? vars)
            (null? hints))
      succeed
      (let ([var (car vars)]
            [hint (car hints)])
        (fresh ()
          (if (zero? hint)
            succeed
            (== var hint))
          (inito (cdr vars) (cdr hints)))))))

(define sudokuo
  (lambda (hints)
    (let* ([vars (mk-vars 81)]
           [rows (split vars 9)]
           [cols (apply map list rows)]
           [sqs (map
                 (lambda (pos)
                   (get-square rows pos 3))
                 '((0 0) (0 3) (0 6)
                   (3 0) (3 3) (3 6)
                   (6 0) (6 3) (6 6)))])
      (run* (q)
        (== q vars)
        (everyg (lambdag@ (var) (domfd var (range 1 9))) vars)
        (inito vars hints)
        (everyg distinctfd rows)
        (everyg distinctfd cols)
        (everyg distinctfd sqs)))))

(pretty-print 
 (sudokuo
  '(0 0 3  0 2 0  6 0 0
      9 0 0  3 0 5  0 0 1
      0 0 1  8 0 6  4 0 0

      0 0 8  1 0 2  9 0 0
      7 0 0  0 0 0  0 0 8
      0 0 6  7 0 8  2 0 0

      0 0 2  6 0 9  5 0 0
      8 0 0  2 0 3  0 0 9
      0 0 5  0 1 0  3 0 0)))
