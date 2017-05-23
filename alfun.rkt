#lang racket

(define (singletons lst)
  
  (define (singletons_helper? n l)
    (member n (remove n l))
    )
  
  (let ([tmp_lst (map (lambda (number)
                        (if (singletons_helper? number lst) null number))
                      lst)])
    (remove* (list null) tmp_lst))
  )


(define (duplicates lst)
  
  (define (duplicates_helper? n1 n2 l)
    (and
     (member n1 (remove n1 l))
     (= (length (member n1 l)) (- (length l) n2))
    )
  )
  
  (let ([tmp_lst (map (lambda (number1 number2)
                        (if (duplicates_helper? number1 number2 lst) number1 null))
                      lst 
                      (build-list (length lst) values))])
    (remove* (list null) tmp_lst))
  )
