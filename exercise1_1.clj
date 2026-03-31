;;Exercise 1.1: Below is a sequence of expressions. What is the result printed by the interpreter in response to each expression? Assume that the sequence is to be evaluated in the order in which it is presented.

10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(def a 3)
(def b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
    
(cond (= a 4) 6
      (= b 4) (+ 6 7 a)
      :else 25)
(+ 2 (if (> b a) b a))
(* (cond (> a b) a
         (< a b) b
         :else -1)
   (+ a 1))

;; Exercise 1.2: Translate the following expression into prefix form:
;; 5 + 4 + ( 2 − ( 3 − ( 6 + 4 5 ) ) ) 3 ( 6 − 2 ) ( 2 − 7 ) .
(/ 
(+ 5 
   4
   (- 2 
      (- 3 
         (+ 6
            (/ 4 5)))))
 
 (* 3 (- 6 2) (- 2 7)) )

;; Exercise 1.3: Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers. 
(defn square [n] (* n n))
(defn sum_squares [n1 n2] (+ (square n1) (square n2)))
(defn sum_squares_of_2_largest [n1 n2 n3];; 
  (cond (and (< n1 n2) (< n1 n3)) (sum_squares n2 n3))
        (and (< n2 n1) (< n2 n3)) (sum_squares n1 n3)
  :else (sum_squares n1 n2))


(sum_squares_of_2_largest 2 2 3)
(sum_squares 2 3)