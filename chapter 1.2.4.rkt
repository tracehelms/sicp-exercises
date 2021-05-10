#lang sicp
; Linear recursive process
; space: O(n), time: O(n)
(define (rec-exponential b n)
  (display ".")
  (if (= n 0)
      1
      (* b (rec-exponential b (- n 1)))))

; Linear process
; space: O(1), time: O(n)
(define (exponential b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (display ".")
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

; space: O(log n), time: O(log n)
(define (fast-exponential b n)
  (display ".")
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exponential b (/ n 2))))
        (else (* b (fast-exponential b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

; Exercise 1.16
; Desired: an interative process of fast-exponential (using successive squaring)
; Desired: space: 0(1), time: 0(log n)
(define (linear-fast-exp b n)
  (linear-fast-exp-iter b n 1))

(define (linear-fast-exp-iter b n a)
  (display ".")
  (cond ((= n 0) a)
        ((even? n) (linear-fast-exp-iter (square b) (/ n 2) a))
        (else (linear-fast-exp-iter b (- n 1) (* b a)))))

; Exercise 1.17
; Turn the below into a fast-exponential version of multiplication
; space: O(n), time: O(log n)
(define (* a b)
  (display ".")
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-mult a b)
  (display ".")
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))