;;;; Arithmetic abstraction

(define-record-type <arithmetic>
    (%make-arithmetic name bases domain-predicate constant-alist
                      operation-alist)
    arithmetic?
  (name arithmetic-name)
  (bases arithmetic-bases)
  (domain-predicate arithmetic-domain-predicate)
  (constant-alist arithmetic-constant-alist)
  (operation-alist arithmetic-operation-alist))

(define (make-arithmetic name
                         domain-predicate
                         bases
                         get-constant
                         get-operation)
  (guarantee predicate? domain-predicate)
  (guarantee-list-of arithmetic? bases)
  (%make-arithmetic
   (cons name (map arithmetic-name bases))
   bases
   domain-predicate
   ;; TODO(cph): Eliding these calls when the number of results
   ;; doesn't equal the number of bases is arbitrary and should
   ;; be reconsidered.
   (filter-map (lambda (name)
                 (let ((base-constants
                        (arithmetic-constants-for name bases)))
                   (and (n:= (length bases)
                             (length base-constants))
                        (cons name
                              (apply get-constant
                                     name
                                     base-constants)))))
               (arithmetic-constant-names-for bases))
   (filter-map (lambda (operator)
                 (let ((base-operations
                        (arithmetic-operations-for operator
                                                   bases)))
                   (and (n:= (length bases)
                             (length base-operations))
                        (cons operator
                              (apply get-operation
                                     operator
                                     base-operations)))))
               (arithmetic-operators-for bases))))

(define (arithmetic-constant-names-for bases)
  (if (n:pair? bases)
      (apply lset-union eq?
             (map arithmetic-constant-names bases))
      (constant-names)))

(define (arithmetic-constants-for name bases)
  (remove default-object?
          (map (lambda (base)
                 (find-arithmetic-constant name base))
               bases)))

(define (arithmetic-operators-for bases)
  (if (n:pair? bases)
      (apply lset-union eq?
             (map arithmetic-operators bases))
      (operator-names)))

(define (arithmetic-operations-for operator bases)
  (filter-map (lambda (base)
                (find-arithmetic-operation operator base))
              bases))

(define (arithmetic-constant-names arithmetic)
  (map car (arithmetic-constant-alist arithmetic)))

(define (arithmetic-constant name arithmetic)
  (let ((constant (find-arithmetic-constant name arithmetic)))
    (if (default-object? constant)
        (error "Unknown constant name:" name arithmetic))
    constant))

;; For use only by generic arithmetic.
(define (arithmetic-constant-binding name arithmetic)
  (let ((binding
         (assq name (arithmetic-constant-alist arithmetic))))
    (if (not binding)
        (error "Unknown constant name:" name arithmetic))
    binding))

(define (find-arithmetic-constant name arithmetic)
  (let ((p (assq name (arithmetic-constant-alist arithmetic))))
    (if p
        (cdr p)
        (default-object))))

(define (arithmetic-operators arithmetic)
  (map car (arithmetic-operation-alist arithmetic)))

(define (arithmetic-operation operator arithmetic)
  (let ((operation
         (find-arithmetic-operation operator arithmetic)))
    (if (not operation)
        (error "Unknown operator:" operator))
    operation))

(define (find-arithmetic-operation operator arithmetic)
  (let ((p (assq operator
                 (arithmetic-operation-alist arithmetic))))
    (and p
         (cdr p))))

(define (add-arithmetics . arithmetics)
  (add-arithmetics* arithmetics))

(define (add-arithmetics* arithmetics)
  (if (n:null? (cdr arithmetics))
      (car arithmetics)
      (make-arithmetic 'add
                       (disjoin*
                        (map arithmetic-domain-predicate
                             arithmetics))
                       arithmetics
                       constant-union
                       operation-union)))

(define (extend-arithmetic extension base-arithmetic)
  (add-arithmetics base-arithmetic (extension base-arithmetic)))

;;;; Installation

(define (install-arithmetic! arithmetic)
  (install-package! (arithmetic->package arithmetic)))

(define (with-arithmetic arithmetic thunk)
  (with-installed-package! (arithmetic->package arithmetic)
                           thunk))

(define (arithmetic->package arithmetic)
  (make-package (arithmetic-name arithmetic)
    (arithmetic->bindings arithmetic
                          (+-like '+ 'additive-identity)
                          (--like '- 'negate)
                          (+-like '* 'multiplicative-identity)
                          (--like '/ 'invert)
                          (comparator '<)
                          (comparator '=)
                          (comparator '>)
                          (comparator '<=)
                          (comparator '>=)
                          (min-like 'min)
                          (min-like 'max))))

(define (arithmetic->bindings arithmetic . modifications)
  (let ((overrides
         (map (lambda (modification)
                (modification arithmetic))
              modifications)))
    (map (lambda (operator)
           (cons operator
                 (make-installable-procedure operator
                                             arithmetic
                                             overrides)))
         (filter operator-installable?
                 (arithmetic-operators arithmetic)))))

(define (make-installable-procedure operator arithmetic
                                    overrides)
  (let* ((operation
          (arithmetic-operation operator arithmetic))
         (procedure (operation-procedure operation)))
    (let ((override
           (and (not (eqv? (get-implementation-value operator)
                           procedure))
                (assq operator overrides))))
      (if override
          (make-installable-operation-procedure procedure
                                                (cdr override))
          procedure))))

(define (+-like operator identity-name)
  (lambda (arithmetic)
    (let ((binary
           (operation-procedure
            (arithmetic-operation operator arithmetic)))
          (get-identity
           (identity-name->getter identity-name arithmetic)))
      (cons operator
            (lambda args
              (case (length args)
                ((0) (get-identity))
                ((1) (car args))
                (else (reduce-left binary #f args))))))))

(define (identity-name->getter identity arithmetic)
  (let ((constant
         (find-arithmetic-constant identity arithmetic)))
    (if (default-object? constant)
        (lambda ()
          (error "No identity for this arithmetic:" identity))
        (lambda ()
          constant))))

(define (--like operator inversion-operator)
  (lambda (arithmetic)
    (let ((binary
           (operation-procedure
            (arithmetic-operation operator arithmetic)))
          (unary
           (operation-procedure
            (arithmetic-operation inversion-operator
                                  arithmetic))))
      (cons operator
            (lambda (arg . args)
              (if (n:pair? args)
                  (reduce-left binary #f (cons arg args))
                  (unary arg)))))))

(define (comparator operator)
  (lambda (arithmetic)
    (let ((binary
           (operation-procedure
            (arithmetic-operation operator arithmetic))))
      (cons operator
            (lambda args
              (or (n:< (length args) 2)
                  (let loop ((args args))
                    (and (binary (car args) (cadr args))
                         (or (not (n:pair? (cddr args)))
                             (loop (cdr args)))))))))))

(define (min-like operator)
  (lambda (arithmetic)
    (let ((binary
           (operation-procedure
            (arithmetic-operation operator arithmetic))))
      (cons operator
            (lambda (arg . args)
              (reduce-left binary #f (cons arg args)))))))