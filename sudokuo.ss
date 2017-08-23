(import
 (chezscheme)
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

(define sudoku-from-str
    (lambda (str)
      (map 
        (lambda (char)
          (if (and (char>? char #\0)
                   (char<=? char #\9))
              (char- char #\0)
              0)) 
        (string->list str))))


(define test
  (lambda (str)
    (time
     (sudokuo
      (sudoku-from-str str)))))

(define test-all
  (lambda (str-list)
    (map test str-list)))

(test-all
 '("85...24..72......9..4.........1.7..23.5...9...4...........8..7..17..........36.4."
   "..53.....8......2..7..1.5..4....53...1..7...6..32...8..6.5....9..4....3......97.."
   "12..4......5.69.1...9...5.........7.7...52.9..3......2.9.6...5.4..9..8.1..3...9.4"
   "...57..3.1......2.7...234......8...4..7..4...49....6.5.42...3.....7..9....18....."
   "7..1523........92....3.....1....47.8.......6............9...5.6.4.9.7...8....6.1."
   "1....7.9..3..2...8..96..5....53..9...1..8...26....4...3......1..4......7..7...3.."
   "1...34.8....8..5....4.6..21.18......3..1.2..6......81.52..7.9....6..9....9.64...2"
   "...92......68.3...19..7...623..4.1....1...7....8.3..297...8..91...5.72......64..."
   ".6.5.4.3.1...9...8.........9...5...6.4.6.2.7.7...4...5.........4...8...1.5.2.3.4."
   "7.....4...2..7..8...3..8.799..5..3...6..2..9...1.97..6...3..9...3..4..6...9..1.35"
   "....7..2.8.......6.1.2.5...9.54....8.........3....85.1...3.2.8.4.......9.7..6...."))



