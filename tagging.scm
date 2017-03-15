;;;; Generic tag access

(define get-tag
  (simple-generic-procedure 'get-tag 1))

(define-generic-procedure-default-handler get-tag
  (lambda (object)
    (implementation-tag object)))

(define (get-predicate object)
  (tag->predicate (get-tag object)))

(define get-data
  (simple-generic-procedure 'get-data 1))

(define-generic-procedure-default-handler get-data
  (lambda (object)
    object))

;;;; Tagged data

(define-record-type <tagged-data>
    (%make-tagged-data tag data)
    tagged-data?
  (tag tagged-data-tag)
  (data tagged-data-data))

(define-generic-procedure-handler get-tag
  (match-args tagged-data?)
  tagged-data-tag)

(define-generic-procedure-handler get-data
  (match-args tagged-data?)
  tagged-data-data)

(define (tagged-data= t1 t2)
  (and (eq? (tagged-data-tag t1) (tagged-data-tag t2))
       (equal*? (tagged-data-data t1) (tagged-data-data t2))))

(define-generic-procedure-handler equal*?
  (match-args tagged-data? tagged-data?)
  tagged-data=)

(define tagged-data-representation
  (make-generic-procedure 'tagged-data-representation 1
                          chaining-generic-dispatcher))

(define-generic-procedure-default-handler
  tagged-data-representation
  (lambda (tagged-data)
    (list (tagged-data-data tagged-data))))

;;; MIT/GNU Scheme: integrate with printer
(set-record-type-unparser-method! <tagged-data>
  (let ((hash (access hash system-global-environment)))
    (lambda (state tagged-data)
      (with-current-unparser-state state
        (lambda (port)
          (display "#[" port)
          (display (tag-name (tagged-data-tag tagged-data)) port)
          (display " " port)
          (write (hash tagged-data) port)
          (for-each (lambda (object)
                      (display " " port)
                      (write object port))
                    (tagged-data-representation tagged-data))
          (display "]" port))))))

(define tagged-data-description
  (make-generic-procedure
   'tagged-data-description 1
   cached-most-specific-generic-dispatcher))

(define-generic-procedure-default-handler
  tagged-data-description
  (lambda (tagged-data) #f))

;;; MIT/GNU Scheme: integrate with pretty-printer
(add-generic-procedure-generator pp-description
  (let ((tag (record-type-dispatch-tag <tagged-data>)))
    (lambda (generic tags)
      generic
      (and (eq? (car tags) tag)
           tagged-data-description))))

;;;; Tagging strategies

(define (tagging-strategy:never name data-test make-tag)

  (define (constructor data)
    (if (not (data-test data))
        (error (string "Ill-formed data for " name ":")
               data))
    data)

  (define tag
    (make-tag data-test constructor (lambda (object) object)))

  tag)

(define (tagging-strategy:always name data-test make-tag)

  (define (constructor data)
    (if (not (data-test data))
        (error (string "Ill-formed data for " name ":")
               data))
    (%make-tagged-data tag data))

  (define (predicate object)
    (and (tagged-data? object)
         (tag<= (tagged-data-tag object) tag)
         (data-test (tagged-data-data object))))

  (define tag
    (make-tag predicate constructor tagged-data-data))

  tag)

(define (tagging-strategy:optional name data-test make-tag)

  (define (constructor data)
    (if (not (data-test data))
        (error (string "Ill-formed data for " name ":")
               data))
    (if (eq? tag (get-tag data))
        data
        (%make-tagged-data tag data)))

  (define (predicate object)
    (or (and (tagged-data? object)
             (tag<= (tagged-data-tag object) tag)
             (data-test (tagged-data-data object)))
        (data-test object)))

  (define (accessor object)
    (if (tagged-data? object)
        (tagged-data-data object)
        object))

  (define tag
    (make-tag predicate constructor accessor))

  tag)