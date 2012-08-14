(define-module (mwitmer shelf-util)
  #:use-module (mwitmer prototype)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-69)
  #:export (leaf-object-map
	    applicator
	    obj-setter
	    matching-instances
	    instance-count
	    make-module-definition
	    leaf-object-fold))

(define (leaf-object-fold obj proc start)
  (hash-table-fold (obj #:op 'children) 
                   (lambda (key value prior)
                     (if (= (hash-table-size (value #:op 'children)) 0) 
                       (proc value prior)
                       (leaf-object-fold value proc prior)))
                   start))

(define (make-module-definition module-name use-modules obj)
 `(define-module ,module-name ,@(map (lambda (use-module) '(#:use-module use-module)) use-modules) #:export (,(obj #:op 'name))))

(define applicator
  (lambda args
    (lambda (obj)
      (apply obj args))))

(define obj-setter
  (lambda (args value)
    (lambda (obj)
      (set! (obj args) value))))

(define (leaf-object-map obj proc)
  (leaf-object-fold obj (lambda (o prior) (cons (proc o) prior)) '()))

(define* (instance-count count parent #:optional properties children #:key args )
  (map (lambda (n) (instance parent properties children #:args args)) (iota count)))

(define (matching-instances pred prop inst)
  (let ((match? (pred inst))
	(child-matches (fold-right append '() 
	       (or (and-let* ((children (inst prop #:def #f)))
			     (map (lambda (child) (matching-instances pred prop child)) children))
		   '()))))
    (if match? (cons inst child-matches) child-matches)))