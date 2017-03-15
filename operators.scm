;;;; Arithmetic operators

(define (operator? object)
  (assq operator %arithmetic-operator-alist))

(define (operator-arity operator)
  (length (operator-domains operator)))

(define (operator-domains operator)
  (cadr (%operator-entry operator)))

(define (operator-codomain operator)
  (caddr (%operator-entry operator)))

(define (operator-installable? operator)
  (not (assq operator internal-operators)))

(define (operator->procedure-name operator)
  (let ((p (assq operator internal-operators)))
    (if p
        (cdr p)
        operator)))

(define internal-operators
  '((negate . -)
    (invert . /)))

(define (constant-names)
  '(additive-identity multiplicative-identity))

(define (operator-names)
  (map car %arithmetic-operator-alist))

(define (%operator-entry operator)
  (or (assq operator %arithmetic-operator-alist)
      (error "Unknown operator:" operator)))

(define (operator-signature operator domain)
  (let ((mapper
         (lambda (indicator)
           (case indicator
             ((domain) domain)
             ((boolean) boolean?)
             ((number) number?)
             (else (error "Unknown domain:" indicator))))))
    (values (map mapper (operator-domains operator))
            (mapper (operator-codomain operator)))))

(define %arithmetic-operator-alist
  '((* (domain domain) domain)
    (+ (domain domain) domain)
    (- (domain domain) domain)
    (/ (domain domain) domain)
    (< (domain domain) boolean)
    (<= (domain domain) boolean)
    (= (domain domain) boolean)
    (> (domain domain) boolean)
    (>= (domain domain) boolean)
    (abs (domain) domain)
    (acos (domain) domain)
    (angle (domain) domain)
    (asin (domain) domain)
    (atan (domain domain) domain)
    (ceiling (domain) domain)
    (cos (domain) domain)
    (exp (domain) domain)
    (expt (domain number) domain)
    (floor (domain) domain)
    (imag-part (domain) domain)
    (invert (domain) domain)
    (log (domain) domain)
    (magnitude (domain) domain)
    (make-polar (domain domain) domain)
    (make-rectangular (domain domain) domain)
    (max (domain domain) domain)
    (min (domain domain) domain)
    (negate (domain) domain)
    (negative? (domain) boolean)
    (positive? (domain) boolean)
    (real-part (domain) domain)
    (remainder (domain domain) domain)
    (round (domain) domain)
    (sin (domain) domain)
    (square (domain) domain)
    (sqrt (domain) domain)
    (tan (domain) domain)
    (truncate (domain) domain)
    (zero? (domain) boolean)))