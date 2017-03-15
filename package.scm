;;;; Packages

;;; A package has a name for use in debugging.
;;; It has a set of named bindings.
;;;   It can produce the set of names it knows about.
;;;   Given a name it can produce the value for that name.
;;; It also has a property list.

(define-record-type <package>
    (%make-package name bindings-alist)
    package?
  (name package-debug-name)
  (bindings-alist package-bindings))

(define (make-package name bindings-alist)
  (guarantee-list-of (is-pair-of n:symbol? any-object?)
                     bindings-alist)
  (%make-package name bindings-alist))

(define (package-names package)
  (map car (package-bindings package)))

(define (package-value package name)
  (let ((p (assq name (package-bindings package))))
    (if p
        (cdr p)
        (error "Unknown binding name:" name))))

(define (similar-packages? p1 p2)
  (lset= eq? (package-names p1) (package-names p2)))

;;; This installer is MIT/GNU Scheme specific:
(define (package-installer environment)
  (lambda (package)
    (make-package
     (list 'uninstall (package-debug-name package))
     (map (lambda (binding)
            (let ((name (car binding))
                  (value (cdr binding)))
              (let ((old-value
                     (environment-lookup environment name)))
                (environment-define environment name value)
                (cons name old-value))))
          (package-bindings package)))))

;;; **** Implement package-uninstaller and uninstall-package!.

;;; THE-ENVIRONMENT is also MIT/GNU Scheme specific:
(define install-package! (package-installer (the-environment)))

(define (with-installed-package! package thunk)
  (let ((old))
    (shallow-fluid-bind (lambda ()
                          (set! old (install-package! package))
                          unspecific)
                        thunk
                        (lambda ()
                          (install-package! (set! old))))))

;;; This is MIT/GNU Scheme specific:
(define-record-printer <package>
  (lambda (package)
    (list (package-debug-name package))))
