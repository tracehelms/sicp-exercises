#lang sicp

; Example: Fibonacci
(define (recursive-fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (recursive-fib (- n 1))
                 (recursive-fib (- n 2))))))

(define (iterative-fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (display ".")
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

; Example: Count Change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        (( or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; Exercise 1.11
(define (recursive-f n)
  (cond ((< n 3) n)
        (else (+ (recursive-f (- n 1))
                 (* 2 (recursive-f (- n 2)))
                 (* 3 (recursive-f (- n 3)))))))

(define (iterative-f n)
  (cond ((< n 3) n)
        (else (iter-f 3 n 2 1 0))))

(define (iter-f current-i n last1 last2 last3)
  (if (> current-i n)
      last1
      (iter-f
       (+ current-i 1)
       n
       (+ last1 (* 2 last2) (* 3 last3))
       last1
       last2)))

; Exercise 1.12
(define (pascals-triangle row column)
  (cond ((> column row) 0)
        ((< column 0) 0)
        ((> column row) 1)
        ((= column 1) 1)
        (else (+ (pascals-triangle (- row 1) (- column 1))
                 (pascals-triangle (- row 1) column)))))












  