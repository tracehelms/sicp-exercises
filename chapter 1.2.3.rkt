#lang sicp
; Exercise 1.14
; My answer: O(n!)
; Correct answer: space: O(n), time: O(n^5)

; Exercise 1.15
; a)
(define (cube x) (* x x x))
(define (p x)
  (display ".")
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 120.15)
; My answer: 5
; b)
; My answer: space: O(n), time: O(n)
; Correct answer: time: O(log(n))

