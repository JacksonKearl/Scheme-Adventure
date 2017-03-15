;;;; Stash copies of numeric arithmetic

(define n:* *)
(define n:+ +)
(define n:- -)
(define n:/ /)
(define n:< <)
(define n:<= <=)
(define n:= =)
(define n:> >)
(define n:>= >=)
(define n:abs abs)
(define n:acos acos)
(define n:angle angle)
(define n:asin asin)
(define n:atan atan)
(define n:ceiling ceiling)
(define n:cos cos)
(define n:exp exp)
(define n:expt expt)
(define n:floor floor)
(define n:imag-part imag-part)
(define n:log log)
(define n:magnitude magnitude)
(define n:make-polar make-polar)
(define n:make-rectangular make-rectangular)
(define n:max max)
(define n:min min)
(define n:negative? negative?)
(define n:positive? positive?)
(define n:real-part real-part)
(define n:remainder remainder)
(define n:round round)
(define n:sin sin)
(define n:sqrt sqrt)
(define n:square square)
(define n:tan tan)
(define n:truncate truncate)
(define n:zero? zero?)

(define n:boolean? boolean?)
(define n:complex? complex?)
(define n:exact-integer? exact-integer?)
(define n:exact-nonnegative-integer? exact-nonnegative-integer?)
(define n:exact-positive-integer? exact-positive-integer?)
(define n:exact-rational? exact-rational?)
(define n:integer? integer?)
(define n:list? list?)
(define n:null? null?)
(define n:number? number?)
(define n:pair? pair?)
(define n:procedure? procedure?)
(define n:rational? rational?)
(define n:real? real?)
(define n:string? string?)
(define n:symbol? symbol?)
(define n:vector? vector?)

(define (compose . args) (compose* args))

(define (compose* args)
  (case (length args)
    ((0) (lambda (x) x))
    ((1) (car args))
    (else (reduce-right (lambda (f g)
                          (lambda (x) (f (g x))))
                        (lambda (x) x)
                        args))))

(define (close-enuf? h1 h2 tolerance)
  (not (> (magnitude (- h1 h2))
          (* .5
             (max tolerance *machine-epsilon*)
             (+ (magnitude h1) (magnitude h2) 2.)))))

(define *machine-epsilon*
  (let loop ((e 1.0))
     (if (= 1.0 (+ e 1.0))
         (* 2.0 e)
         (loop (/ e 2.0)))))

(define (sign x)
  (cond ((positive? x) 1)
        ((negative? x) -1)
        (else 0)))

(define (hash-table-intern! table key get-value)
  (or (hash-table-ref/default table key #f)
      (let ((value (get-value)))
        (hash-table-set! table key value)
        value)))

;;;; List utilities

(define (non-empty-list? object)
  (and (n:pair? object)
       (n:list? (cdr object))))
(define n:non-empty-list? non-empty-list?)

(define (all-permutations-of items)
  (guarantee n:list? items)
  (let loop ((items items))
    (if (n:pair? items)
        (append-map (lambda (index)
                      (map (let ((head (list-ref items index)))
                             (lambda (tail)
                               (cons head tail)))
                           (loop (delete-item items index))))
                    (iota (length items)))
        '(()))))

(define (delete-item items index)
  (append (take items index)
          (cdr (drop items index))))

(define (elementwise-lists-of lists)
  (guarantee-list-of n:non-empty-list? lists)
  (let loop ((lists lists))
    (if (n:pair? lists)
        (append-map (let ((tails (loop (cdr lists))))
                      (lambda (head)
                        (map (lambda (tail)
                               (cons head tail))
                             tails)))
                    (car lists))
        '(()))))

(define (partition-by-key = get-key objects)
  (let ((partitions '()))
    (for-each (lambda (object)
                (let ((key (get-key object)))
                  (let ((partition
                         (find (lambda (partition)
                                 (= (car partition) key))
                               partitions)))
                    (if partition
                        (set-cdr! partition
                                  (cons object (cdr partition)))
                        (set! partitions
                              (cons (list key object)
                                    partitions))))))
              objects)
    partitions))

;;; MIT/GNU Scheme implementation specific:

(define (implementation-type-name object)
  (let ((names
         (microcode-type/code->names (object-type object))))
    (if (n:pair? names)
        (car names)
        'object)))

(define (implementation-type-predicate name)
  (or (hash-table-ref/default %implementation-type-predicates
                              name
                              #f)
      (let ((predicate
             (let ((code (microcode-type name)))
               (lambda (object)
                 (object-type? code object)))))
        (hash-table-set! %implementation-type-predicates
                         name
                         predicate)
        predicate)))

(define %implementation-type-predicates
  (make-strong-eq-hash-table))

(define (has-implementation-value? name)
  (environment-bound? system-global-environment name))

(define (get-implementation-value name)
  (environment-lookup system-global-environment name))

;;; Needed for SRFI 69:
(define hash
  (access hash (->environment '(runtime hash-table))))

(define (hash-by-eqv object #!optional modulus)
  (if (default-object? modulus)
      (eqv-hash object)
      (eqv-hash-mod object modulus)))

(define (save-environment! name environment)
  ;; A hook to use if we want to keep track of
  ;; loaded environments.  Called by the loader.
  name)

(define load-quietly
  (cond ((environment-bound? system-global-environment
                             'param:suppress-loading-message?)
         ;; Post-9.2 parameters:
         (lambda args
           (parameterize ((param:suppress-loading-message? #t))
             (apply load args))))
        ((environment-bound? system-global-environment 'let-fluid)
         ;; Unreleased "fluid" implementation, now defunct.  This
         ;; clause should be deleted as soon as GJS updates
         ;; scmutils to latest HEAD (or reverts to 9.2).
         (lambda args
           (let-fluid load/suppress-loading-message?
                      #t
                      (lambda () (apply load args)))))
        (else
         ;; Older shallow fluid bindings:
         (lambda args
           (fluid-let ((load/suppress-loading-message? #t))
             (apply load args))))))

(define (define-record-printer record-type get-parts)
  (set-record-type-unparser-method!
   record-type
   (simple-unparser-method
    (let ((name (record-type-name record-type)))
      (if (and (string-prefix? "<" name)
               (string-suffix? ">" name))
          (substring name 1 (fix:- (string-length name) 1))
          name))
    get-parts)))