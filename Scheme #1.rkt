#|
Author: Andre P. Exilien
Date: 20 Sept 2023
Scheme #1
|#

#lang r5rs

 ;;Part : 1

 ;;  7 + 3 + 2
(define (run1)
  (+ 7 3 2))

;; 7 * (12 + 18) + 3
(define (run2)
  (+ (* 7 (+ 12 18)) 3))

;; ((18 * 8) / 6) + (17 * (18 – 3)) - 9
(define (run3)
  (+ (- (/ (* 18 8) 6) 9) (* 17 (- 18 3))))

;; (7 * (4 - 9)) / ((11 + 11) - 15) - (6 + 9)
(define (run4)
  (- (/ (* 7 (- 4 9)) (- (+ 11 11) 15)) (+ 6 9)))

;; ((10/((2+16)-13))*(((10+4)*(17-8)))) / ((((14-15)+10)/(3+9))*(72/9))
(define (run5)
  (/ (* (/ 10 (- (+ 2 16) 13)) (* (+ 10 4) (- 17 8))) 
     (* (/ (+ (- 14 15) 10) (+ 3 9)) (/ 72 9))))


  ;;Part : 2A

;; Damage Calculation
(define (ffMQ-Damage attack defense weakness targets)
  (floor (/ (* (- (* attack 4) defense) weakness) targets)))
 ;; Step 1: Start by multiplying the attack value by 4
;; Step 2: Subtract the defense value from the multiplied attack.
;; Step 3: Adjust the attack strength based on the enemy's weakness.
;; Step 4: Divide the total damage by the number of targets.
;; Step 5: Round down the final damage to get the damage value.
   







;;Part 2B


;; FF10 Base Damage Calculation
(define (ff10-baseDamage stat dmgValue)
  (floor(* (+ (/ (expt stat 3) 32) 32) (/ dmgValue 16))))
  
  ;; Step 1: we do  the cube of the 'stat' value using (stat^3)
  ;; Step 2: we divide this cubed value by 32 using (/ (expt stat 3) 32)
  ;; Step 3: we add 32 to the result using (+ ... 32)
  ;; Step 4: we divide 'dmgValue' by 16 using (/ dmgValue 16)
  ;; Step 5: we multiply these two results together using
  ;; which gives us our final base damage.
  #|
Part 2C
|# 
;; FF10 Defense Number Calculation
(define (ff10-defenseNum defense) (round(+ (/ (expt (- defense 280.4) 2) 110) 16)))

  ;; Step 1: subtract 280.4 from the 'defense' value
  ;; Step 2 : square this result
  ;; Step 3: divide the squared value by 110
  ;; Step 4:  add 16 to the result

;;Part 2D


;; FF10 Reduced Damage Calculation
(define (ff10-reduceDamage baseDmg defenseNum)
  (floor (/ (* baseDmg defenseNum) 730)))

 ;; Step 1: multiply 'baseDmg' * 'defenseNum'
 ;; Step 2: divide  result by 730  













;;Part 2E




;; FF10 Final Damage Calculation
(define (ff10-finalDamage reducedDmg defense)
  (let ((step1 (* defense 51))
        (step2 (/ (expt defense 2) 11)))
    (* reducedDmg (/ (- 730 (- step1 step2)) 730))))


  
  ;; Step 1: multiply 'defense' by 51 using (* defense 51)
  ;; Step 2: square 'defense' using (expt defense 2) and divide it by 11
  ;; Step 3: with the answer from the result is subtracted from the initial multiplication 
  ;; Step 4: we subtract this entire result from 730 using (- 730 ...)
  ;; Step 5: we multiply the 'reducedDmg' by this value and divide by 730 to get the final damage.




;;Part 2F


;; FF10 Damage Calculation All together 

(define (ff10-calculate-damage stat dmgValue defense)
  (ff10-finalDamage
    (ff10-reduceDamage
      (ff10-baseDamage stat dmgValue)   ; Step 1
      (ff10-defenseNum defense))        ; Step 2
    defense))                            ; Step 3 & 4
;;
;; Step 1: Calculate the base damage for the given stat and dmgValue.
;; Step 2: Calculate the defense number for the given defense value.
;; Step 3: Calculate the reduced damage using the base damage and defense number.
;; Step 4: Return the final damage using the reduced damage and defense.
  
;;Part 3A & B




(define factorial
  (lambda (n)
    (cond
          ((< n 0) 'nan) ;; Base case: Return 'NAN for negative numbers.
          ((= n 0) 1)  ;; Base case: 0! is defined as 1.
          (else (* n (factorial (- n 1)))))))   ;; Recursive case: n! is n times (n-1)!



;; Alternating Factorial Function
;; Computes the absolute value of the alternating sum of the first n factorials.

(define alt-factorial
  (lambda (n)
    (cond
          ((<= n 0) (factorial n))  ;; Base case: For n <= 0, return the factorial value of n.
          ((= n 1) 1) ;; Base case: af(1) is 1.
          (else (- (factorial n) (alt-factorial (- n 1)))))))  ;; Recursive case: Sub alt factorial of n-1 from n! Which will return alt - factorial of n.


;;4A,B,C,D,E




;; Part A:
;; returns 2nd item of the list.
(define get-second-item 
  (lambda (lst)
    (car (cdr lst)))) 

;; Part B:
;; returns the 3rd item of the list.
(define get-third-item 
  (lambda (lst)
    (car (cdr (cdr lst)))))

;; Part C:
;;  recursively count the number of items in the list.
(define count-items 
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (count-items (cdr lst))))))

;; returns the count of items in the list.
(define list-length?
  (lambda (lst)
    (count-items lst)))

;; Part D:
;;  get the cdr starting at the item number passed in.
(define get-arbitrary-cdr
  (lambda (num lst)
    (if (= num 0)
        lst
        (get-arbitrary-cdr (- num 1) (cdr lst)))))

;; return the desired cdr or #f if the number is larger than list size.
(define arbitrary-cdr 
  (lambda (num lst)
    (if (> num (list-length? lst))
        #f
        (get-arbitrary-cdr (- num 1) lst))))


;;Part E
(define make-list-helper 
  (lambda (value size)
   
    (if (= size 0)
        '() ;; If the size is 0, return an empty list.
         (cons value (make-list-helper value (- size 1))))))    ;; Otherwise, cons the value onto the recursive call with decremented size.

;; Main function to create a list of given size and value.
(define make-list
  (lambda (value size)
    ;; If size is not a number, return an empty list.
    (if (number? size)
        (make-list-helper value size)
        '())))


;;5A&B



;; Part A: Checks is the list only numbers
(define number-list?
  (lambda (lst)
      (if (null? lst)
        #f ;; If empty, print #f

        ;; If All in list is a number, print  #t
        (cond ((not (number? (car lst))) #f)
              ((null? (cdr lst)) #t)
              (else (number-list? (cdr lst)))))))









;; Part B multiplying numbers in the list
(define multiply-list-helper
  (lambda (lst)
    (if (null? lst)
        1
        (* (car lst) (multiply-list-helper (cdr lst))))))


;; Main function: multiply-number-list
(define multiply-number-list
  (lambda (lst)
    (if (number-list? lst)
        (multiply-list-helper lst)
        #f)))


;;Part 6 




;; Function that takes two functions and an integer, and checks which function provides a greater value
(define which-function
  (lambda (func1 func2 n)
    (let ((result1 (func1 n))
          (result2 (func2 n)))
      (cond ((> result1 result2) 1)
            ((< result1 result2) 2)
            (else 0)))))

;; Sample functions for testing
(define increment (lambda (n) (+ n 1)))
(define decrement (lambda (n) (- n 1)))
(define identity (lambda (n) n))
