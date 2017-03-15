;;;; Templates for parametric predicates

(define-record-type <predicate-template>
    (%make-predicate-template name
                              pattern
                              instantiator
                              predicate)
    predicate-template?
  (name predicate-template-name)
  (pattern predicate-template-pattern)
  (instantiator predicate-template-tag-instantiator)
  (predicate predicate-template-predicate))
(register-predicate! predicate-template? 'predicate-template)

(define (make-predicate-template name pattern tagging-strategy
                                 make-data-test)
  (guarantee template-pattern? pattern)
  (letrec*
      ((instantiator
        (make-predicate-template-tag-instantiator
         name
         pattern
         make-data-test
         tagging-strategy
         (lambda () template)))
       (predicate
        (lambda (object)
          (and (parametric-predicate? object)
               (eqv? (parametric-predicate-template object)
                     template))))
       (template
        (%make-predicate-template
         name
         pattern
         (simple-list-memoizer equal?
                               hash
                               (lambda parameters parameters)
                               instantiator)
         predicate)))
    (register-predicate! predicate (symbol name '-predicate))
    (set-predicate<=! predicate parametric-predicate?)
    template))

(define (make-predicate-template-tag-instantiator
           name pattern make-data-test tagging-strategy
           get-template)
  (lambda patterned-tags
    (letrec
        ((tag
          (make-parametric-tag
           (cons name
                 (map-template-pattern pattern
                                       patterned-tags
                                       tag-name))
           (make-data-test (lambda () tag))
           tagging-strategy
           (get-template)
           (match-template-pattern pattern
                                   patterned-tags
                                   tag?))))
      tag)))

(define (predicate-template-instantiator template)
  (let ((tag-instantiator
         (predicate-template-tag-instantiator template))
        (pattern (predicate-template-pattern template)))
    (lambda patterned-predicates
      (tag->predicate
       (apply tag-instantiator
              (map-template-pattern pattern
                                    patterned-predicates
                                    predicate->tag))))))

(define (predicate-template-parameter-names template)
  (template-pattern->names
   (predicate-template-pattern template)))

(define (predicate-template-accessor name template)
  (let ((elt
         (find (lambda (elt)
                 (eq? (template-pattern-element-name elt)
                      name))
               (predicate-template-pattern template))))
    (if (not elt)
        (error "Unknown parameter name:" name template))
    (let ((valid? (predicate-template-predicate template))
          (convert
           (if (template-pattern-element-single-valued? elt)
               tag->predicate
               tags->predicates)))
      (lambda (predicate)
        (guarantee valid? predicate)
        (convert
         (parameter-binding-value
          (find (lambda (binding)
                  (eqv? name (parameter-binding-name binding)))
                (parametric-tag-bindings
                 (predicate->tag predicate)))))))))

;;;; Template patterns

(define (template-pattern? object)
  (and (n:non-empty-list? object)
       (every template-pattern-element? object)
       (list-of-unique-symbols?
        (template-pattern->names object))))

(register-predicate! template-pattern? 'template-pattern)

(define (template-pattern-element? object)
  (and (n:pair? object)
       (template-pattern-operator? (car object))
       (n:pair? (cdr object))
       (template-pattern-name? (cadr object))
       (or (n:null? (cddr object))
           (and (n:pair? (cddr object))
                (polarity? (caddr object))
                (n:null? (cdddr object))))))

(define (template-pattern-operator? object)
  (memq object '(? ?* ?+)))

(define (template-pattern-name? object)
  (and (n:symbol? object)
       (not (template-pattern-operator? object))
       (not (polarity? object))))

(define (polarity? object)
  (memq object '(+ = -)))

(define (template-pattern-element-operator element)
  (car element))

(define (template-pattern-element-name element)
  (cadr element))

(define (template-pattern-element-polarity element)
  (if (n:null? (cddr element))
      '+
      (caddr element)))

(define (template-pattern-element-single-valued? element)
  (eq? '? (template-pattern-element-operator element)))

(define (template-pattern->names pattern)
  (map template-pattern-element-name pattern))

(define (match-template-pattern pattern values value-predicate)
  (guarantee n:list? values)
  (if (not (n:= (length values) (length pattern)))
      (error "Wrong number of values:" values pattern))
  (map (lambda (element value)
         (case (template-pattern-element-operator element)
           ((?)
            (if (not (value-predicate value))
                (error "Mismatch:" element value)))
           ((?*)
            (if (not (and (n:list? value)
                          (every value-predicate value)))
                (error "Mismatch:" element value)))
           ((?+)
            (if (not (and (n:non-empty-list? value)
                          (every value-predicate value)))
                (error "Mismatch:" element value)))
           (else
            (error:not-a template-pattern? pattern)))
         (make-parameter-binding element value))
       pattern
       values))

(define-record-type <parameter-binding>
    (make-parameter-binding element value)
    parameter-binding?
  (element parameter-binding-element)
  (value parameter-binding-value))

(define (parameter-binding-name binding)
  (template-pattern-element-name
   (parameter-binding-element binding)))

(define (parameter-binding-polarity binding)
  (template-pattern-element-polarity
   (parameter-binding-element binding)))

(define (parameter-binding-values binding)
  (if (template-pattern-element-single-valued?
       (parameter-binding-element binding))
      (list (parameter-binding-value binding))
      (parameter-binding-value binding)))

(define (map-template-pattern pattern object value-procedure)
  (map (lambda (element o)
         (case (template-pattern-element-operator element)
           ((?) (value-procedure o))
           ((?* ?+) (map value-procedure o))
           (else (error:not-a template-pattern? pattern))))
       pattern
       object))