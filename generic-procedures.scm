;;;; Generic procedures

(define generic-procedure?)
(define generic-procedure-metadata)
(define set-generic-procedure-metadata!)
(let ((association (make-metadata-association)))
  (set! generic-procedure? (association 'has?))
  (set! generic-procedure-metadata (association 'get))
  (set! set-generic-procedure-metadata! (association 'put!)))

(define (make-generic-procedure name arity dispatcher)
  (let* ((metadata
          (make-generic-metadata name
                                 arity
                                 (dispatcher)
                                 (get-default-handler name)))
         (procedure
          (lambda args
            (generic-procedure-dispatch metadata args))))
    (set-generic-procedure-metadata! procedure metadata)
    procedure))

(define (simple-generic-procedure name arity)
  (make-generic-procedure name arity simple-generic-dispatcher))

(define (get-default-handler name)
  (lambda args
    (error "Inapplicable generic procedure:" name args)))

(define (generic-procedure-dispatch metadata args)
  (let ((handler (get-generic-procedure-handler metadata args)))
    (if trace-generic-dispatch?
        (trace-generic-dispatch metadata args handler))
    (apply handler args)))

(define (trace-generic-dispatch metadata args handler)
  (fluid-let ((trace-generic-dispatch? #f))
    (let ((port (trace-output-port)))
      (fresh-line port)
      (display ";Calling method of " port)
      (display (generic-metadata-name metadata) port)
      (display ": " port)
      (write handler port)
      (for-each (lambda (arg)
                  (display " " port)
                  (write arg port))
                args)
      (newline port))))

(define trace-generic-dispatch? #f)

(define (get-generic-procedure-handler metadata args)
  (or ((generic-metadata-getter metadata) args)
      ((generic-metadata-default-getter metadata))))

(define (define-generic-procedure-handler proc applicability
                                          handler)
  (((generic-metadata-dispatcher
     (generic-procedure-metadata proc))
    'add-handler!)
   applicability
   handler))

(define (define-generic-procedure-default-handler proc handler)
  (((generic-metadata-dispatcher
     (generic-procedure-metadata proc))
    'set-default-handler!)
   handler))

(define (generic-procedure-name proc)
  (generic-metadata-name (generic-procedure-metadata proc)))

(define (generic-procedure-arity proc)
  (generic-metadata-arity (generic-procedure-metadata proc)))

(define (generic-procedure-rules proc)
  (((generic-metadata-dispatcher
     (generic-procedure-metadata proc))
    'get-rules)))

(define (generic-procedure-handlers proc)
  (map cdr (generic-procedure-rules proc)))

;;;; Metadata

(define-record-type <generic-metadata>
    (%make-generic-metadata name arity dispatcher getter
                            default-getter)
    generic-metadata?
  (name generic-metadata-name)
  (arity generic-metadata-arity)
  (dispatcher generic-metadata-dispatcher)
  (getter generic-metadata-getter)
  (default-getter generic-metadata-default-getter))

(define (make-generic-metadata name arity dispatcher
                               default-handler)
  ((dispatcher 'set-default-handler!) default-handler)
  (%make-generic-metadata name
                          arity
                          dispatcher
                          (dispatcher 'get-handler)
                          (dispatcher 'get-default-handler)))

;;;; Dispatcher implementations

(define (simple-generic-dispatcher)
  (let ((rules '())
        (default-handler #f))

    (define (get-handler args)
      (let ((rule
             (find (lambda (rule)
                     (predicates-match? (car rule) args))
                   rules)))
        (and rule
             (cdr rule))))

    (define (add-handler! applicability handler)
      (for-each (lambda (predicates)
                  (let ((p (assoc predicates rules)))
                    (if p
                        (set-cdr! p handler)
                        (set! rules
                              (cons (cons predicates handler)
                                    rules)))))
                applicability))

    (define (get-default-handler)
      default-handler)

    (define (set-default-handler! handler)
      (set! default-handler handler))

    (lambda (message)
      (case message
        ((get-handler) get-handler)
        ((add-handler!) add-handler!)
        ((get-default-handler) get-default-handler)
        ((set-default-handler!) set-default-handler!)
        ((get-rules) (lambda () rules))
        (else (error "Unknown message:" message))))))

(define (trie-generic-dispatcher)
  (let ((base-dispatcher (simple-generic-dispatcher))
        (trie (make-trie)))

    (define (get-handler args)
      (get-a-value trie args))

    (define (add-handler! applicability handler)
      ((base-dispatcher 'add-handler!) applicability handler)
      (for-each (lambda (path)
                  (set-path-value! trie path handler))
                applicability))

    (lambda (message)
      (case message
        ((get-handler) get-handler)
        ((add-handler!) add-handler!)
        (else (base-dispatcher message))))))

(define (make-cached-generic-dispatcher base-dispatcher get-key)
  (let ((get-handler
         (simple-list-memoizer eqv?
                               hash-by-eqv
                               (lambda (args) (map get-key args))
                               (base-dispatcher 'get-handler))))
    (lambda (message)
      (case message
        ((get-handler) get-handler)
        (else (base-dispatcher message))))))

(define (cached-generic-dispatcher get-key)
  (make-cached-generic-dispatcher (simple-generic-dispatcher)
                                  get-key))

;;; Extensible equality predicate.
(define equal*? (simple-generic-procedure 'equal*? 2))
(define-generic-procedure-default-handler equal*? equal?)

(define equal*-predicate
  (equality-predicate-maker 'equal*? equal*?))
