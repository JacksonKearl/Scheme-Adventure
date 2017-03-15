(define (make-subsetting-generic-dispatcher
         make-effective-handler)
  (lambda ()
    (let ((delegate (simple-generic-dispatcher)))

      (define (get-handler args)
        (let ((matching
               (filter (lambda (rule)
                         (is-generic-handler-applicable?
                          rule args))
                       ((delegate 'get-rules)))))
          (and (n:pair? matching)
               (make-effective-handler
                (map cdr (sort matching rule<))
                ((delegate 'get-default-handler))))))

      (lambda (operator)
        (case operator
          ((get-handler) get-handler)
          (else (delegate operator)))))))

(define (is-generic-handler-applicable? rule args)
  (if (simple-function? (cdr rule))
      (simple-function-apply-fit (cdr rule) args)
      (predicates-match? (car rule) args)))

(define (rule< r1 r2)
  (let loop ((ps1 (car r1)) (ps2 (car r2)))
    (if (pair? ps1)
        (cond ((eqv? (car ps1) (car ps2))
               (loop (cdr ps1) (cdr ps2)))
              ((predicate<= (car ps1) (car ps2))
               #t)
              ((predicate<= (car ps2) (car ps1))
               #f)
              (else
               (loop (cdr ps1) (cdr ps2))))
        #f)))

(define most-specific-generic-dispatcher
  (make-subsetting-generic-dispatcher
   (lambda (handlers default-handler)
     (car handlers))))

(define chaining-generic-dispatcher
  (make-subsetting-generic-dispatcher
   (lambda (handlers default-handler)
     (let loop ((handlers handlers))
       (if (pair? handlers)
           (lambda args
             (apply (car handlers)
                    (loop (cdr handlers))
                    args))
           default-handler)))))

(define (cached-most-specific-generic-dispatcher)
  (make-cached-generic-dispatcher
   (most-specific-generic-dispatcher)
   get-tag))

(define (cached-chaining-generic-dispatcher)
  (make-cached-generic-dispatcher (chaining-generic-dispatcher)
                                  get-tag))

;;; API
(define generic-dispatcher
  cached-most-specific-generic-dispatcher)

(define (std-generic-procedure name arity)
  (make-generic-procedure
   name arity cached-most-specific-generic-dispatcher))

(define (chaining-generic-procedure name arity)
  (make-generic-procedure name arity
                          cached-chaining-generic-dispatcher))