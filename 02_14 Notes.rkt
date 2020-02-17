#lang racket
; sumnumbers:takes a list of atos and sums the numbers in ths list.
(define sumnumbers
  (lambda (lis)
    (cond
      [(null? lis) 0]
      [(number? (car lis)) (+ (car lis) (sumnumbers (cdr lis)))]
      [else (sumnumbers (cdr lis))])))

; CPS form
(define sumnumbers-cps
  (lambda (lis return)
    (cond
      [(null? lis) (return 0)]
      [(number? (car lis)) (sumnumbers-cps (cdr lis) (lambda (v)) (return (+ v (car lis))))]
      [else (sumnumbers-cps (cdr lis) return)])))

;sumnumbers* :sum the number sin a list that contains sublists
(define sumnumbers
  (lambda (lis)
    (cond
      [(null? lis) 0]
      [(list? (car lis)) ( + ( sumnumbers* cdr lis))]
      [(number? (car lis)) (+ (car lis)(sumnumbers* (cdr lis)))])
      [else (sumnumnbers.cps (cdr lis))]))

(define sumnumbers*-cps
  (lambda (lis return)
    (cond
      [(null? lis) (return 0)]
      [(list? (car lis)) (sumnumbbers*-cps (car lis) (lambda (v1)
                                                       (lambda (v1) (sumnumbers*-cps (cdr lis)
                                                                                     (lambda (v2)
return (+ v1 v2))))))]))
[(number? (car lis)) (sumnumbers*-cps (cdr lis) lambda (v) return +y(car lis))])
[else (sumnumbers*-cps(cdr lis) return )]))))



;removeall*-cps: remove all copies of a x from a list of litss
;flatten-cps )need append-cps '((a)(b((((((((((c d ))))))) ==> (ABCD)


(define removeall*-cps
  (lambda (x lis return)
    (cond
      [(null? lis) (return '())]
      [(list? (car lis)) ]
      [ (eq? (car lis) x)]
      