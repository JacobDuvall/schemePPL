#lang racket

; Author: Jacob Duvall
; Date: April 8, 2019

; revlist function from class. Takes an integer list and reverses its order. 
(define (revlist l) (if (null? l) l (append (revlist (cdr l))(list (car l)))))

;rem0atb function from class. Takes an integer list a removes leading 0. 
(define (rem0atb l1) (if (null? l1) l1 (if (= (car l1 ) 0) (rem0atb (cdr l1)) l1)))

;ladd function from class. Takes two integer lists and adds them together. 
(define (ladd l1 l2) (if (null? l1) (rem0atb l2) (if (null? l2) (rem0atb l1) (revlist(rem0atb(revlist(cons (+ (car l1) (car l2)) (ladd (cdr l1) (cdr l2)))))))))

;lsub function. Takes two integer lists and subtracts l1 from l2. 
(define (lsub l1 l2) (if (null? l1) (rem0atb l2) (if (null? l2) (rem0atb l1) (revlist(rem0atb(revlist(cons (- (car l1) (car l2)) (lsub (cdr l1) (cdr l2)))))))))

;get_car. Replaces '() with '(0)
(define (get_car l1) (if (null? (car l1)) (car '((0))) (car l1)))

;poly_add function. Takes two lists of integer lists and adds apol to bpol.
(define (poly_add apol bpol) (if (null? apol) bpol (if (null? bpol) apol (cons (ladd (get_car apol) (get_car bpol)) (poly_add (cdr apol) (cdr bpol))))))

;poly_sub function. Takes two lists of integer lists and subtracts apol from bpol.
(define (poly_sub apol bpol) (if (null? apol) bpol (if (null? bpol) apol (cons (lsub (get_car apol) (get_car bpol)) (poly_sub (cdr apol) (cdr bpol))))))

;poly_derx function. Takes a list of integer lists and calculates derivative.
(define (poly_derx apol) (if (null? apol) '() (cons (derivative_helper(car apol)) (poly_derx (cdr apol)))))

;derivative_helper function. Calculates derivative.
(define (derivative_helper apol) (if (null? apol) '() (rem0atb(revlist(cons(* (car(revlist apol)) (- (count apol) 1)) (derivative_helper (throwaway apol)))))))

;count function. Counts the degree of the polynomial.
(define (count apol) (if (null? apol) 0 (+ 1 (count (cdr apol))) ))

;throwaway function. Throws away the last element of list.
(define (throwaway apol)(if (null? (cdr apol)) '() (cons (car apol)(throwaway(cdr apol)))))

;trailingzero function. Throws away '() elements at the end. 
(define (trailingzero apol) (if (null? apol) (cdr apol) apol))

;poly_mul function. Takes two lists of integer lists and multiplies them.
(define (poly_mul apol bpol) (if (null? apol) bpol (if (null? bpol) apol (cons(lmult (car apol) (car bpol)) (poly_mul (cdr apol) (cdr bpol))))))

(define (lmult apol bpol) (if (null? apol) bpol (if (null? bpol) apol (revlist(rem0atb(revlist(* (get_car apol)  (get_car apol) (lmult (cdr apol) (cdr apol)))))))))


;(poly_add '( (1 -1) (1 2 3) () (3))  '((-1 1) (-1 2) (3)))
;(poly_sub '( (1 -1) (1 2 3) () (3))  '((-1 1) (-1 2) (3)))
;(poly_derx  '( (1) (1 2 3) () (3)))
;
