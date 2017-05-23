#lang racket

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
;; #reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname estimate)  
;; (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; YOUR HEADER HERE

(define random-number/big 4294967087)

;; (random-number min max) produces a random number between min and max
;; random-number: Num Num -> Num
;; Examples:
;; (random-number -1 1) -> 0.56
;; (random-number 0 0.5) -> 0.2

(define (random-number min max)
   (+ min (* (- max min)
             (/ (random random-number/big)
                random-number/big))))

(define (estimate-pi n)
  (cond
    [(equal? n 0) 0]
    [else
     (cond
       [(> (+ (sqr (random-number -1 1)) (sqr (random-number -1 1))) 1) (+ 0 (/ (* (estimate-pi (- n 1)) (- n 1)) n))]
       [else (/ (* (+ 1 (/ (* (estimate-pi (- n 1)) (- n 1)) 4)) 4) n)])
     ]))
