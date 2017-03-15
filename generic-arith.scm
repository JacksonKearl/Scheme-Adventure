;;;; Generic arithmetic

(define (make-generic-arithmetic dispatcher)
  (make-arithmetic 'generic any-object? '()
    (lambda (name)
      (constant-union name))
    (lambda (operator)
      (simple-operation operator
                        any-object?
                        (make-generic-procedure
                          operator
                          (operator-arity operator)
                          dispatcher)))))

(define (add-to-generic-arithmetic! generic-arithmetic
                                    arithmetic)
  ;; TODO: We have a choice here: do we merge constants with
  ;; non-standard names into the generic arithmetic?  For now, we
  ;; will ignore such constants.
  (for-each
   (lambda (name)
     (let ((binding
            (arithmetic-constant-binding name
                                         generic-arithmetic))
           (element (find-arithmetic-constant name arithmetic)))
       (set-cdr! binding
                 (constant-union name
                                 (cdr binding)
                                 element))))
   (arithmetic-constant-names generic-arithmetic))
  (for-each
   (lambda (operator)
     (let ((generic-procedure
            (simple-operation-procedure
             (arithmetic-operation operator
                                   generic-arithmetic))))
       (for-each (lambda (operation)
                   (define-generic-procedure-handler
                       generic-procedure
                       (operation-applicability operation)
                       (operation-procedure operation)))
                 (operation-components
                  (arithmetic-operation operator arithmetic)))))
   (arithmetic-operators arithmetic)))

(define (extend-generic-arithmetic! generic-arithmetic extension)
  (add-to-generic-arithmetic! generic-arithmetic
                              (extension generic-arithmetic)))