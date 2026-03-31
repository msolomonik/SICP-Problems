#lang sicp


; Exercise 1.3: Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers. 
(define (square n) (* n n))
(define (sum_squares a b) (+ (square a) (square b)))
(define (less_or_eq a b) (or (= a b) (< a b)))
(define (first_is_smallest a b c) 
    (and (less_or_eq a b) (less_or_eq a c)))
(define (sum_squares_larger_two a b c) 
    (cond ((first_is_smallest a b c) (sum_squares b c)) 
          ((first_is_smallest b a c) (sum_squares a c)) 
          (else (sum_squares a b))))

; (sum_squares_larger_two 1 2 3)      ; Expected: 13
; (sum_squares_larger_two 3 2 1)      ; Expected: 13
; (sum_squares_larger_two 1 3 2)      ; Expected: 13
; (sum_squares_larger_two 5 5 5)      ; Expected: 50
; (sum_squares_larger_two 1 1 2)      ; Expected: 5
; (sum_squares_larger_two 2 1 1)      ; Expected: 5
; (sum_squares_larger_two 0 1 2)      ; Expected: 5
; (sum_squares_larger_two -1 2 3)     ; Expected: 13
; (sum_squares_larger_two 4 -2 1)     ; Expected: 17
; (sum_squares_larger_two -3 -1 -2)   ; Expected: 5
; (sum_squares_larger_two 10 5 7)     ; Expected: 149
; (sum_squares_larger_two 0 0 1)      ; Expected: 1

;; Exercise 1.4: Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:
; (define (a-plus-abs-b a b)
;   ((if (> b 0) + -) a b))

;; ANSWER
;; the if clause is used to determine the operator (+ or -) 
;; which is to be used on arguments a and b
;; so if b > 0 then (a-plus-abs-b a b) evaluates to (+ a b)
;; otherwise (- a b)

; Exercise 1.5: Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

(define (p) (p))

(define (test x y) 
  (if (= x 0) 
      0 
      y))

; Then he evaluates the expression

; (test 0 (p))

; What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer. (Assume that the evaluation rule for the special form if is the same whether the interpreter is using normal or applicative order: The predicate expression is evaluated first, and the result determines whether to evaluate the consequent or the alternative expression.)

;; ANSWER:
;; applicative-order:
;; the interpreter attempts to first evaluate the operands
;; so it tries to evaluate (p), and to do so must evaluate (p)
;; and so on. It is an infinite loop, so the program gets stuck
;; normal-order evaluation:
;; the interpreter wants to first expand (p) it does so to (p) a single time
;; then it expands (test 0 (p)) into the full body with the if statement
;; because (= x 0) evaluates to true, it can return 0 without tryiing to evaluate (p)
;; so with normal-order evaluation it evaluates to 0 without getting stuck

; Exercise 1.6: Alyssa P. Hacker doesn’t see why if needs to be provided as a special form. “Why can’t I just define it as an ordinary procedure in terms of cond?” she asks. Alyssa’s friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:

(define (new-if predicate 
                then-clause 
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; Eva demonstrates the program for Alyssa:

; (new-if (= 2 3) 0 5)
; 5

; (new-if (= 1 1) 0 5)
; 0

; Delighted, Alyssa uses new-if to rewrite the square-root program:


(define (average x y) 
  (/ (+ x y) 2))

; (define (good-enough? guess x)
;   (< (abs (- (square guess) x)) 0.001))
; (define (improve guess x)
;   (average guess (/ x guess)))
; (define (sqrt-iter guess x)
;   (new-if (good-enough? guess x)
;           guess
;           (sqrt-iter (improve guess x) x)))

; What happens when Alyssa attempts to use this to compute square roots? Explain. 
; (sqrt-iter 1 4)
; It gets stuck in an infinite loop. The reason is due to the lack of
; short-circuit logic built into the special form of if.
; Typically, we'll avoid an infinite recursion loop in sqrt-iter
; because at some point (good-enough? guess x) will be true
; so the interpreter will know to return guess without trying to evaluate
; the recursive call to sqrt-iter. In this case, however, due to the applicative-order
; evaluation, it will keep trying to evaluate the recursive call forever.

; Exercise 1.7: The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers. An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers? 
; (define (sqrt-iter guess x)
;   (if (good-enough? guess x)
;       guess
;       (sqrt-iter (improve guess x) x)))
; (sqrt-iter 1.0 0.000064)
; (sqrt-iter 1.0 64000)
; ** Answer **
; You can see in the examples, above they produce the answers 0.03 and 253
; which are slightly inaccurate. This shows how the sqrt-iter function
; previously implemented does not work accurately for very small or very
; large numbers. 

;; ** New implementation **
(define (good-enough? guess old-guess)
  (< (abs (- guess old-guess)) 0.00001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (define new-guess (improve guess x))
  (if (good-enough? guess new-guess)
      guess
      (sqrt-iter new-guess x)))

; (sqrt-iter 2.0 64)
; (improve 4.001219512195122 16)

; --- Test cases for Exercise 1.7 ---
; Small numbers
; (sqrt-iter 1.0 0.000064)    ; Expected: ~0.008
; (sqrt-iter 1.0 0.0001)      ; Expected: ~0.01
; (sqrt-iter 1.0 0.00000001)  ; Expected: ~0.0001

; ; Large numbers
; (sqrt-iter 1.0 64000)       ; Expected: ~252.98
; (sqrt-iter 1.0 1000000)     ; Expected: ~1000
; (sqrt-iter 1.0 9999999999)  ; Expected: ~99999.99999

; ; Perfect squares
; (sqrt-iter 1.0 4)           ; Expected: 2
; (sqrt-iter 1.0 9)           ; Expected: 3
; (sqrt-iter 1.0 144)         ; Expected: 12

; ; Different starting guesses
; (sqrt-iter 100.0 16)        ; Expected: ~4 (starting far above)
; (sqrt-iter 0.001 16)        ; Expected: ~4 (starting far below)
; (sqrt-iter 1000000.0 25)    ; Expected: ~5 (wildly high start)

; ; Edge-ish cases
; (sqrt-iter 1.0 1)           ; Expected: 1
; (sqrt-iter 1.0 2)           ; Expected: ~1.4142
; (sqrt-iter 1.0 0.5)         ; Expected: ~0.7071

; Exercise 1.8: Newton’s method for cube roots is based on the fact that if y is an approximation to the cube root of x , then a better approximation is given by the value
; (x / y^2 + 2y) / 3 .
; Use this formula to implement a cube-root procedure analogous to the square-root procedure.
(define (improve-cube-root y x)
  (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (cube-root-iter guess x)
  (define new-guess (improve-cube-root guess x))
  (if (good-enough? guess new-guess)
      guess
      (cube-root-iter new-guess x)))

; --- Test cases for Exercise 1.8 ---
; Perfect cubes
(cube-root-iter 1.0 8)           ; Expected: 2
(cube-root-iter 1.0 27)          ; Expected: 3
(cube-root-iter 1.0 64)          ; Expected: 4
(cube-root-iter 1.0 125)         ; Expected: 5
(cube-root-iter 1.0 1000)        ; Expected: 10

; Small numbers
(cube-root-iter 1.0 0.001)       ; Expected: 0.1
(cube-root-iter 1.0 0.000008)    ; Expected: 0.02
(cube-root-iter 1.0 0.000000027) ; Expected: 0.0003

; Large numbers
(cube-root-iter 1.0 1000000)     ; Expected: 100
(cube-root-iter 1.0 8000000000)  ; Expected: 2000

; Non-perfect cubes
(cube-root-iter 1.0 2)           ; Expected: ~1.2599
(cube-root-iter 1.0 10)          ; Expected: ~2.1544
(cube-root-iter 1.0 100)         ; Expected: ~4.6416

; Negative numbers
(cube-root-iter -1.0 -8)         ; Expected: -2
(cube-root-iter -1.0 -27)        ; Expected: -3

; Different starting guesses
(cube-root-iter 100.0 8)         ; Expected: 2 (starting far above)
(cube-root-iter 0.001 27)        ; Expected: 3 (starting far below)

; Edge case
(cube-root-iter 1.0 1)           ; Expected: 1