(define-module (shelf shelf)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-42)
  #:use-module (ice-9 receive)
  #:use-module (system vm objcode)
  #:use-module (system base compile)
  #:export 
    (method
    enclose
    runtime-prototype-update?
    enable-cross-references?
    set-uid-generator!
    default-uid-generator
    resolve-reference
    make-reference
    reference?
    make-bytecode-record
    bytecode-record->objcode
    bytecode-record?
    bytecode-record-bytecode
    h
    m
    define-object
    define-objects
    define-object-public
    define-objects-public
    object
    extend
    instance))

(define runtime-prototype-update? #t)
(define enable-cross-references? #t)

(define (set-uid-generator! generator) 
  "Sets the uid generator for new objects to thunk @var{generator}."
  (set! uid-generator generator))

(define (default-uid-generator)
  "Generate a uid of the format
xxxxxxxx-xxxx-4xxx-[8-b]xxx-xxxxxxxxxxxx where each x is a random hex
digit"
  (let ((random-digits 
	 (lambda (n)
	   (string-concatenate
	    (map (lambda (rnd) (number->string rnd 16))
		 (list-ec (: i n) (random 16 shelf-random-state)))))))
    (string-append 
     (random-digits 8) "-" 
     (random-digits 4) "-4" 
     (random-digits 3) "-" 
     (number->string (+ (random 4 shelf-random-state) 8) 16) (random-digits 3) "-" 
     (random-digits 12))))

(define uid-generator default-uid-generator)

(define (array-map proc arr)
  "Creates an array with the same dimensions as @var{arr} and with
each element the result of calling @var{proc} on its corresponding
element in @var{arr}."
  (let ((new-arr (apply make-array `(,*unspecified* ,@(array-dimensions arr)))))
    (array-index-map! new-arr 
		      (lambda indices 
			(let ((r (apply array-ref `(,arr ,@indices)))) (proc r))))
    new-arr))

(define (property->syntax el) 
  "Generate the appropriate serialized syntax for @var{el}."
  (cond 
   ((module? el) (module->syntax el))
   ((method? el) (method-syntax el))
   ((hash-table? el) (hash-table->syntax el))
   ((bytecode-record? el) `(make-bytecode-record ,(bytecode-record-bytecode el)))
   ((procedure? el) (el #:op 'serialize))
   ((reference? el) (if (resolve-reference el)
	      	`(make-reference ,(reference-uid el) #f)
			#f))
   ((list? el) `(list ,@(map property->syntax el)))
   ((vector? el) `(vector ,@(map property->syntax (vector->list el))))
   ((or (string? el) (bitvector? el) (not (array? el)) (symbol? el)) `(quote ,el))
   (else 
    (let ((els (array->list (array-map (lambda (r) `(,'unquote ,(property->syntax r))) el))))
      `(list->array 
	,(length (array-dimensions el)) 
	,(list 'quasiquote els))))))

(define (module->syntax mod)
  "Generate the syntax for a macro to generate module @var{mod}"
  `(m ,@(hash-fold
	 (lambda (key value sofar)
	   (cons `(,key ,(property->syntax (variable-ref value))) sofar)) 
	 '() (module-obarray mod))))

(define (hash-table->syntax htable)
  "Generate the syntax for a macro to generate hash table @var{htable}"
  `(h ,@(hash-fold
	 (lambda (key value sofar)
	   (cons `(,key ,(property->syntax value)) sofar)) '() htable)))

(define (obj-lookup obj key restrict-module-lookup?)
  "Lookup up @var{key} in @var{obj}. If @var{restrict-module-lookup?}
is true, a module will only lookup @var{key} up in its obarray;
otherwise, it will look up its whole inheritance chain."
  (let get-match ((keys (if (list? key) key (list key)))
		  (obj obj))
    (let* ((key (car keys))
	   (match
	     (cond
	      ((vector? obj) (vector-ref obj key))
	      ((array? obj) (apply array-ref `(,obj ,@key)))
	      ((module? obj) 
	       (if restrict-module-lookup?
		   (hashq-ref (module-obarray obj) key *unspecified*)
		   (module-ref obj key *unspecified*)))
	      ((hash-table? obj) (hashq-ref obj key *unspecified*))
	      ((reference? obj) ((resolve-reference obj) #:op 'get key))
	      ((procedure? obj) (obj #:op 'get key)))))
      (if (or (= (length keys) 1) (eq? *unspecified* match)) match (get-match (cdr keys) match)))))

(define (get-both key obj default restrict-module-lookup?)
  (let ((my-result (obj-lookup (obj #:op 'props) key restrict-module-lookup?)))
    (if (eq? my-result *unspecified*)
	(if (obj #:op 'parent)
	    (get-both key (obj #:op 'parent) default restrict-module-lookup?)
	    (if (eq? default *unspecified*)
		(error "Object property not found: " key)
		(values #f default)))
	(values obj my-result))))

(define (get-containing-object key obj)
  "Get the object in @var{obj}'s inheritance chain that actually
contains @var{key}, #f if there is no match."
  (receive (obj value) (get-both key obj #f #t) obj))

(define (get key obj default) 
  "Lookup @var{key} in Shelf object @var{obj}, returning @var{default}
if there is no match"
  (receive (obj value) (get-both key obj default #f) value))

(define (my-set! key value props)
  (if (list? key)
      (let hash-set-inner! ((inner-props props) (keys key))
	(if (= 1 (length keys))
	    (cond
	     ((array? inner-props)
	      (apply array-set! `(,inner-props ,value ,@(car keys))))
	     ((module? inner-props)
	      (module-set! inner-props (car keys) value))
	     (else
	      (hashq-set! inner-props (car keys) value)))
	    (let ((next-inner-props 
		   (cond 
		    ((array? inner-props)
		     (apply array-ref `(,inner-props ,@(car keys))))
		    ((module? inner-props)
		     (module-ref inner-props (car keys) *unspecified*))
		    (else
		     (hash-ref inner-props (car keys) *unspecified*)))))
	      (if (eq? *unspecified* next-inner-props)
		  (let ((new-inner-props (make-hash-table)))
		    (hashq-set! inner-props (car keys) new-inner-props)
		    (hash-set-inner! new-inner-props (cdr keys)))
		  (hash-set-inner! next-inner-props (cdr keys))))))
      (module-define! props key value)))

(define (desc name child-hash)
  (let ((children 
	 (hash-fold (lambda (key value sofar) (cons value sofar)) '() child-hash)))
    (fold 
     (lambda (child match) 
       (if match match 
	   (if (eq? name (child #:op 'name)) 
	       child 
	       (desc name (child #:op 'children))))) #f children)))

(define-syntax m
  (syntax-rules ()
    ((_ (key val) ...)
     (let ((mod (make-module)))
       (module-define! mod (quasiquote key) val) ...
       mod))))

(define-syntax h
  (syntax-rules ()
    ((_ (key val) ...)
     (let ((htable (make-hash-table)))
       (hashq-set! htable (quasiquote key) val) ...
       htable))))

(define bytecode-record (make-record-type 'bytecode '(bytecode)))

(define bytecode-record? (record-predicate bytecode-record))
(define make-bytecode-record (record-constructor bytecode-record))
(define bytecode-record-bytecode (record-accessor bytecode-record 'bytecode))
(define (bytecode-record->objcode bc) (bytecode->objcode (bytecode-record-bytecode bc)))

(define method-record (make-record-type 'method '(procedure syntax)))

(define method? (record-predicate method-record))
(define make-method (record-constructor method-record))
(define method-syntax (record-accessor method-record 'syntax))
(define method-procedure (record-accessor method-record 'procedure))

(define reference-record (make-record-type 'reference '(uid obj)))

(define reference? (record-predicate reference-record))
(define make-reference (record-constructor reference-record))
(define reference-uid (record-accessor reference-record 'uid))
(define reference-obj (record-accessor reference-record 'obj))
(define reference-obj-set! (record-modifier reference-record 'obj))

(define (resolve-reference ref)
  (if (not (reference-obj ref))
      (let ((ht (make-weak-value-hash-table))
	    (obj (hash-ref uids (reference-uid ref))))
	(hash-set! ht 'obj obj)
	(reference-obj-set! ref ht)
	obj)
      (let ((ht (reference-obj ref)))
	(hash-ref ht 'obj))))

(define-syntax method
  (lambda (x)
    (syntax-case x ()
      ((_ args stmt ...)
       (with-syntax
	   ((this (datum->syntax x 'this))
	    (super (datum->syntax x 'super)))
	 (syntax (make-method 
		  (lambda (this super) (lambda* args stmt ...)) 
		  (quote (method args stmt ...)))))))))

(define-syntax enclose
  (lambda (x)
    (syntax-case x ()
      ((_ ((name value) ...) args stmt ...)
       (with-syntax
	   ((this (datum->syntax x 'this))
	    (super (datum->syntax x 'super)))
	 (syntax (make-method
		  (let ((name value) ...) (lambda (this super) (lambda* args stmt ...)))
		  `(enclose ((name ,(property->syntax value)) ...) args stmt ...))))))))

(define (remove-keyword-args args keywords)
  (cdr (pair-fold 
	(lambda (pair prev) 
	  (if (car prev) `(#f . ,(cdr prev)) 
	      (if (find (lambda (keyword) (eq? keyword (car pair))) keywords)
		  `(#t . ,(cdr prev)) 
		  `(#f . ,(append (cdr prev) (list (car pair))))))) '(#f . ()) args)))

(define (extend parent spec)
  (for-each 
   (lambda (spec-el) 
     (let ((child (if (list? spec-el) 
		      (begin (extend (car spec-el) (cadr spec-el))
			     (car spec-el))
		      spec-el)))       
       (child #:op 'set-parent! parent)
       (hashq-set! (parent #:op 'children) (child #:op 'name) child))) spec))

(define uids
  (make-weak-value-hash-table))

(define (make-super child parent)
  (if 
   parent      
   (lambda* (#:key (def *unspecified*) #:allow-other-keys . args)	       
     (let* ((parent-clean-args (remove-keyword-args args (list #:op #:def)))
	    (direct-parent (get-containing-object (car parent-clean-args) parent)))
       (let process-super-val ((val (get (car parent-clean-args) (direct-parent #:op 'parent) def)))
	 (cond
	  ((method? val) 
	   (apply ((method-procedure val) child (make-super child (direct-parent #:op 'parent)))
		  (cdr parent-clean-args)))
	  ((reference? val) (resolve-reference val))
	  ((list? val) (map (lambda (el) (process-super-val el)) val))
	  (else val)))))
   #f))

(define (process-val val me parent args)
  (cond
   ((method? val) (apply ((method-procedure val) me (make-super me parent)) (cdr args)))
   ((reference? val) (resolve-reference val))
   ((list? val) (map (lambda (el) (process-val el me parent args)) val))
   (else val)))

(define-syntax extract-object-names
  (syntax-rules ()
    ((_ (n ((key value) ...)))
     n)
    ((_ (n ((key value) ...) (child ...)))
     (list n (list (extract-object-names child) ...)))))

(define-syntax make-object-hierarchy
  (syntax-rules ()
    ((_ (child ...))
     (list (extract-object-names child) ...))
    ((_ n ((key value) ...) (child ...))
     (extend n (make-object-hierarchy (child ...))))))

(define-syntax only-define-object
  (syntax-rules ()
    ((_ (n ((key value) ...)))
     (define-object n (key value) ...))
    ((_ (n ((key value) ...) (child ...)))
     (begin
       (define-object n (key value) ...)
       (only-define-object child) ...))))

(define-syntax only-define-object-public
  (syntax-rules ()
    ((_ (n ((key value) ...)))
     (define-object-public n (key value) ...))
    ((_ (n ((key value) ...) (child ...)))
     (begin
       (define-object-public n (key value) ...)
       (only-define-object-public child) ...))))

(define-syntax define-objects-public
  (syntax-rules ()
    ((_ n ((key value) ...) (child ...))
     (begin
       (only-define-object-public (n ((key value) ...) (child ...)))
       (make-object-hierarchy n ((key value) ...) (child ...))))))

(define-syntax define-objects
  (syntax-rules ()
    ((_ n ((key value) ...) (child ...))
     (begin
       (only-define-object (n ((key value) ...) (child ...)))
       (make-object-hierarchy n ((key value) ...) (child ...))))))

(define-syntax define-object-public
  (syntax-rules ()
    ((_ n (key value) ...) 
     (begin (define-public n (object (quote n) (m (key value) ...)))
	    (if runtime-prototype-update?
		(update-existing-objects (quote n) n))))))

(define-syntax define-object
  (syntax-rules ()
    ((_ n (key value) ...) 
     (begin (define n (object (quote n) (m (key value) ...)))
	    (if runtime-prototype-update?
		(update-existing-objects (quote n) n))))))

(define (update-existing-objects name obj)
 (hash-for-each 
  (lambda (uid obj)
    (let ((parent (obj #:op 'parent)))
      (if (and parent (eq? name (parent #:op 'name))) 
	  (obj #:op 'set-parent! obj))))
  uids))

(define* (object #:optional n properties children-list #:key saved-uid)
  (letrec* 
      ((children (make-hash-table))
       (props (if properties properties (make-module)))
       (parent #f)
       (name n)
       (uid 
	(if (or enable-cross-references? runtime-prototype-update?)
	    (or saved-uid (uid-generator)) 
	    #f))       
       (me 
	(make-procedure-with-setter 
	 (lambda* 
	     (#:key op (def *unspecified*) #:allow-other-keys . args)
	   (let ((clean-args 
		  (if (or op (not (eq? *unspecified* def))) 
		      (remove-keyword-args args (list #:op #:def)) 
		      args)))
	     (case op
	       ((#f) (let ((val (get (car clean-args) me def))) 
		       (process-val val me parent clean-args)))
	       ((set-parent!) 
		(set! parent (car clean-args))
		(hash-clear! (module-import-obarray props))
		(set-module-uses! props 
				  (if parent
				      (list (parent #:op 'props))
				      '())))
	       ((get-raw) (get (car clean-args) me def))
	       ((uid) uid)
	       ((reference) 
		(if enable-cross-references? 
		    (let ((ht (make-weak-value-hash-table)))
		      (hash-set! ht 'obj me)
		      (make-reference uid ht))
		    (error "Shelf references not enabled (set \"enable-cross-references\" to #t)")))
	       ((get) (let ((result (get (car clean-args) me def)))
			(cond 
			 ((method? result) ((method-procedure result) me (make-super me parent)))
			 ((reference? result) (resolve-reference result))
			 (else result))))
	       ((desc) (if (eq? name (car clean-args)) me (desc (car clean-args) children)))
	       ((serialize) 		   
		`(,@(if (eq? name *unspecified*) 
			(list 'instance (if parent (parent #:op 'name) #f))
			(list 'object `(quote ,name)))
		  ,(module->syntax props)		      
		  (list ,@(hash-fold 
			   (lambda (key value sofar) 
			     (cons (value #:op 'serialize) sofar)) 
			   '() children))
		  ,@(if enable-cross-references? (list #:saved-uid uid) '())))
	       ((compile) 
		(make-bytecode-record 
		 (compile (me #:op 'serialize) 
			  #:to 'bytecode 
			  #:env (car clean-args))))
	       ((parent) parent)
	       ((document) 
		(hash-fold
		 (lambda (key val prev) 		      
		   (if (method? (variable-ref val))
		       (assq-set! prev key
				  (procedure-documentation 
				   ((method-procedure (variable-ref val)) #f #f))))) 
		 (if parent (parent #:op 'document) '()) (module-obarray props)))
	       ((children) children)
	       ((props) props)
	       ((name) name)
	       (else (error "Can't handle args: " args)))))
	 (lambda (key value) (my-set! key value props)))))
    (if (or enable-cross-references? runtime-prototype-update?) (hash-set! uids uid me))
    (if (list? children-list) 
	(map (lambda (child) 
	       (child #:op 'set-parent! me) (hashq-set! children (child #:op 'name) child))
	     children-list))
    me))

(define* (instance par #:optional properties children #:key args saved-uid)
  (let ((child (object *unspecified* properties children #:saved-uid saved-uid))
	(parent (cond
		 ((pair? par) ((car par) #:op 'desc (cdr par)))
		 (else par))))
    (child #:op 'set-parent! parent)
    (if args (apply child (cons 'initialize! args))) child))

(define shelf-random-state (random-state-from-platform))