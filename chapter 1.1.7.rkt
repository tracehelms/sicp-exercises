#lang sicp
(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.00001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-iter guess x)
  (display "sqrt-iter") (newline)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (alt-good-enough? guess last-guess)
  (< (abs (- guess last-guess)) 0.00001))

(define (alt-sqrt-iter guess x last-guess)
  (display "alt-sqrt-iter") (newline)
  (if (alt-good-enough? guess last-guess)
      guess
      (alt-sqrt-iter (improve guess x)
                 x guess)))

(define (alt-sqrt x) (alt-sqrt-iter (* x 0.25) x 0))

(define (cube-root x) (cube-root-iter x 1.0))

(define (cube-good-enough? x guess)
  (< (abs (- (* guess guess guess) x)) 0.00001))

(define (cube-improve x guess)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cube-root-iter x guess)
  (if (cube-good-enough? x guess)
      guess
      (cube-root-iter x (cube-improve x guess)
                      )))

(define (simplified-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))