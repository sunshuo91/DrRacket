#lang racket

(define-struct pprof (title wts) #:transparent)

(define (applicant-score lst ppf)
  (cond
    [(not(equal? (length lst) (length (pprof-wts ppf)))) "input error"]
    [else
     (cond
       [(empty? lst) 0]
       [(equal? lst 0) 0]
       [(empty? ppf) 0]
       [else (+ (* (first lst) (first (pprof-wts ppf))) (applicant-score (rest lst) (pprof (pprof-title ppf) (rest (pprof-wts ppf)))))])]))

(define (position-max lst ppf1 ppf2)
  (if (< (applicant-score lst ppf1) (applicant-score lst ppf2)) ppf2 ppf1))

(define (position-list-max lst plst)
  (cond
    [(empty? plst) empty]
    [else (position-max lst (first plst) (position-list-max lst (rest plst)))]))

(define (remove-position ppf plst)
  (cond
    [(empty? plst) empty]
    [else (if (equal? ppf (first plst)) (remove-position ppf (rest plst)) (cons (first plst) (remove-position ppf (rest plst))))]))