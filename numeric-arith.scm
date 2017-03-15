;;;; Numeric arithmetic

(define numeric-arithmetic
  (make-arithmetic 'numeric number? '()
    (lambda (name)
      (case name
        ((additive-identity) 0)
        ((multiplicative-identity) 1)
        (else (default-object))))
    (lambda (operator)
      (simple-operation operator
                        number?
                        (get-implementation-value
                         (operator->procedure-name operator))))))
