;;;; Collections

(define (make-weak-eq-set)
  (let ((elements '()))

    (define (get-elements)
      (weak-list->list elements))

    (define (has-element? element)
      (if (weak-memq element elements) #t #f))

    (define (add-element! element)
      (if (not (weak-memq element elements))
          (begin
            (set! elements (weak-cons element elements))
            unspecific)))

    (lambda (operator)
      (case operator
        ((get-elements) get-elements)
        ((has-element?) has-element?)
        ((add-element!) add-element!)
        (else (error "Unknown operator:" operator))))))

(define (make-alist-store key=?)
  (let ((alist '()))

    (define (get-keys)
      (map car alist))

    (define (has? key)
      (any (lambda (p)
             (key=? (car p) key))
           alist))

    (define (get key)
      (let ((p
             (find (lambda (p)
                     (key=? (car p) key))
                   alist)))
        (if (not p)
            (error "Unknown key:" key))
        (cdr p)))

    (define (get-matching predicate)
      (filter-map (lambda (p)
                    (and (predicate (car p))
                         (cdr p)))
                  alist))

    (define (put! key datum)
      (set! alist
            (cons (cons key datum)
                  (remove! (lambda (p)
                             (key=? (car p) key))
                           alist)))
      key)

    (lambda (operator)
      (case operator
        ((get-keys) get-keys)
        ((has?) has?)
        ((get) get)
        ((get-matching) get-matching)
        ((put!) put!)
        (else (error "Unknown operator:" operator))))))

(define (make-hash-table-store make-table)
  (let ((table (make-table)))

    (define (get-keys)
      (hash-table-keys table))

    (define (has? key)
      (hash-table-exists? table key))

    (define (get key)
      (hash-table-ref table key))

    (define (put! key metadata)
      (hash-table-set! table key metadata))

    (lambda (operator)
      (case operator
        ((get-keys) get-keys)
        ((has?) has?)
        ((get) get)
        ((put!) put!)
        (else (error "Unknown operator:" operator))))))

(define (make-metadata-association)
  (let* ((store
          (make-hash-table-store make-key-weak-eqv-hash-table))
         (base-has? (store 'has?))
         (base-get (store 'get))
         (base-put! (store 'put!)))

    (define (put! key metadata)
      (if (base-has? key)
          (let ((metadata* (base-get key)))
            (if (not (eqv? metadata* metadata))
                (error "Can't change metadata for:"
                       key metadata metadata*))))
      (base-put! key metadata))

    (lambda (operator)
      (case operator
        ((put!) put!)
        (else (store operator))))))