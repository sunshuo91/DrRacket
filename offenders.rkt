#lang racket

(define-struct email (staff-id rec w-count reply))

(define email7 (make-email 1 (list 2 3) 5 empty))
(define email6 (make-email 2 (list 1 4 5) 10 (list email7)))
(define email5 (make-email 3 (list 6 7) 15 (list email6)))
(define email4 (make-email 1 (list 2 3 4 5 6 7) 20 empty))
(define email3 (make-email 2 (list 3 5 6) 25 empty))
(define email2 (make-email 3 (list 4 7) 30 empty))
(define email1 (make-email 1 (list 2 3 4 5 6 7) 35 (list email2 email3 email4)))



(define (total-word-count em) 
  (define (word-count-helper lst)
    (cond
      [(empty? lst) 0]
      [else (+ (email-w-count (first lst)) (word-count-helper (rest lst)))]
      )
    )
  
  (+ (email-w-count em) (word-count-helper (email-reply em)))
  )


(define (unique-email-senders email-lst)
  (define (remove-dup lst)
    (cond
      [(empty? lst) empty]
      [(empty? (rest lst)) lst]
      [else
       (if (equal? (first lst) (first (rest lst)))
           (remove-dup (rest lst))
           (cons (first lst) (remove-dup (rest lst))))]
      ))
  
  (define (unique-email-senders-helper lst)
    (cond
      [(empty? lst) empty]
      [(empty? (rest lst)) (list (email-staff-id (first lst)))]
      [else (cons (email-staff-id (first lst)) (unique-email-senders-helper (rest lst)))]
      )
    )
  
  (cond
    [(empty? email-lst) empty]
    [else (remove-dup (sort 
                       (append 
                        (append 
                         (list (email-staff-id (first email-lst))) 
                         (unique-email-senders-helper (email-reply (first email-lst))))
                        (unique-email-senders (rest email-lst))) <
                                                                 ))] 
    ))

(define (sent-email-summary email-lst2)
  (define (arrange-dup lst)
    (cond
      [(empty? lst) empty]
      [(empty? (rest lst)) (cons (list (first lst) 1) empty)]
      [else
       (if (equal? (first lst) (first (rest lst)))
           (cons (list (first lst) (+ (list-ref (first (arrange-dup (rest lst))) 1) 1)) 
                 (rest (arrange-dup (rest lst))))
           (cons (list (first lst) 1) (arrange-dup (rest lst))))]
      ))
  
  (define (unique-email-senders-helper lst)
    (cond
      [(empty? lst) empty]
      [(empty? (rest lst)) (list (email-staff-id (first lst)))]
      [else (cons (email-staff-id (first lst)) (unique-email-senders-helper (rest lst)))]
      )
    )
  
  (cond
    [(empty? email-lst2) empty]
    [else (arrange-dup (sort 
                        (append 
                         (append 
                          (list (email-staff-id (first email-lst2))) 
                          (unique-email-senders-helper (email-reply (first email-lst2))))
                         (unique-email-senders (rest email-lst2))) <
                                                                   ))] 
    ))


(define (email-offenders email-lst3 thr)
  (define (email-offenders-helper? lst)
    (> (last lst) thr))
  
  (define (email-offenders-helper lst)  
    (cond
      [(empty? lst) empty]
      [else (if (email-offenders-helper? (first lst))
                (cons (first (first lst)) (email-offenders-helper (rest lst)))
                (email-offenders-helper (rest lst)))]
      ))
  
  (email-offenders-helper (sent-email-summary email-lst3)))
  