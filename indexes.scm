;;;; Indexes: sequence masks

(define (index-limit length)
  (n:expt 2 length))

(define (index-all length)
  (n:- (index-limit length) 1))

(define (index-predicate length)
  (lambda (object)
    (and (n:exact-nonnegative-integer? object)
         (n:< object (index-limit length)))))

(define (index->booleans index length)
  (let loop ((i 0) (index index) (choices '()))
    (if (n:< i length)
        (loop (n:+ i 1)
              (quotient index 2)
              (cons (odd? index) choices))
        choices)))