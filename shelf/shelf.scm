(define-module (shelf shelf)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (ice-9 receive)
  #:use-module (system vm objcode)
  #:use-module (system base compile)
  #:export   
  (:
   bytecode-record->objcode
   bytecode-record-bytecode
   bytecode-record?
   define-object
   define-objects
   define-object-public
   define-objects-public
   enable-cross-references?
   enclose
   extend
   h
   instance
   m
   make-bytecode-record
   make-reference
   method
   object
   object-children
   object-compile
   object-documentation
   object-get
   object-get-raw
   object-instances
   object-name
   object-parent
   object-props
   object-reference
   object-serialize
   object-uid
   reference?
   resolve-reference
   runtime-prototype-update?
   set-uid-generator!
   set-object-children!
   set-object-name!
   set-object-parent!
   set-object-props!
   set-object-uid!
   super))

(define runtime-prototype-update? #t)
(define enable-cross-references? #t)
(define shelf-random-state (random-state-from-platform))

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
		 (map (lambda (n) (random 16 shelf-random-state)) (iota n)))))))
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

(define uids
  (make-weak-value-hash-table))

(define object-record 
  (make-record-type 
   'object 
   '(name parent props children uid instances) 
   (lambda (rec port) 
     (if (object-parent rec) 
	 (format port "<object ~a (instance of ~a)>"
		 (object-name rec)
		 (object-name (object-parent rec)))
	 (format port "<object ~a>" 
		 (object-name rec))))))

(define set-object-name! (record-modifier object-record 'name))

(define (set-object-parent! obj parent) 
  ((record-modifier object-record 'parent) obj parent)
  (hash-set! (object-instances parent) (object-uid obj) obj)
  (hash-clear! (module-import-obarray (object-props obj)))
  (set-module-uses! (object-props obj) 
		    (if parent
			(list (object-props parent))
			'())))

(define set-object-props! (record-modifier object-record 'props))
(define set-object-children! (record-modifier object-record 'children))
(define set-object-uid! (record-modifier object-record 'uid))

(define object? (record-predicate object-record))

(define object-name (record-accessor object-record 'name))
(define object-parent (record-accessor object-record 'parent))
(define object-props (record-accessor object-record 'props))
(define object-instances (record-accessor object-record 'instances))
(define object-children (record-accessor object-record 'children))
(define object-uid (record-accessor object-record 'uid))

(define make-object (record-constructor object-record))

(define (obj-lookup obj key restrict-module-lookup?)
  "Lookup up @var{key} in @var{obj}. If @var{restrict-module-lookup?}
is true, a module will only lookup @var{key} up in its obarray;
otherwise, it will look up its whole inheritance chain."
  (let get-match ((keys (if (list? key) key (list key)))
		  (obj obj))
    (let* ((key (car keys))
	   (match
	     (cond
	      ((array? obj) (apply array-ref obj (if (list? key) key (list key))))
	      ((module? obj) 
	       (if restrict-module-lookup?
		   (hashq-ref (module-obarray obj) key *unspecified*)
		   (module-ref obj key *unspecified*)))
	      ((hash-table? obj) (hashq-ref obj key *unspecified*))
	      ((reference? obj) (object-get (resolve-reference obj) key)))))
      (if (or (= (length keys) 1) (eq? *unspecified* match)) match (get-match (cdr keys) match)))))

(define (remove-keyword-args args keywords)
  (cdr (pair-fold 
	(lambda (pair prev) 
	  (if (car prev) `(#f . ,(cdr prev)) 
	      (if (find (lambda (keyword) (eq? keyword (car pair))) keywords)
		  `(#t . ,(cdr prev)) 
		  `(#f . ,(append (cdr prev) (list (car pair))))))) '(#f . ()) args)))

(define-syntax method
  (lambda (x)
    (syntax-case x ()
      ((_ args stmt ...)
       (with-syntax
	   ((this (datum->syntax x 'this)))
	 (syntax (make-method 
		  (lambda (this) (lambda* args stmt ...)) 
		  (quote (method args stmt ...)))))))))

(define-syntax enclose
  (lambda (x)
    (syntax-case x ()
      ((_ ((name value) ...) args stmt ...)
       (with-syntax
	   ((this (datum->syntax x 'this)))
	 (syntax (make-method
		  (let ((name value) ...) (lambda (this) (lambda* args stmt ...)))
		  `(enclose ((name ,(property->syntax value)) ...) args stmt ...))))))))

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

(define (generalized-set key value collection) 
  (cond
   ((array? collection)
    (apply array-set! collection value (if (list? key) key (list key))))
   ((module? collection)
    (module-define! collection key value))
   (else
    (hashq-set! collection key value))))

(define (generalized-get key collection)
  (cond 
   ((array? collection) 
    (apply array-ref `(,collection (if (list? key) key (list key)))))
   ((module? collection) 
    (module-ref collection key *unspecified*))
   (else 
    (hashq-ref collection key *unspecified*))))

(define (object-set! key value props)
  (if (list? key)
      (let hash-set-inner! ((inner-props props) (keys key))
	(if (= 1 (length keys))
	    (generalized-set (car keys) value inner-props)
	    (let ((next-inner-props (generalized-get (car keys) inner-props)))
	      (if (eq? *unspecified* next-inner-props)
		  (let ((new-inner-props (make-hash-table)))
		    (generalized-set (car keys) new-inner-props inner-props)
		    (hash-set-inner! new-inner-props (cdr keys)))
		  (hash-set-inner! next-inner-props (cdr keys))))))
      (module-define! props key value)))

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

(define (property->syntax el) 
  "Generate the appropriate serialized syntax for @var{el}."
  (cond 
   ((module? el) (module->syntax el))
   ((method? el) (method-syntax el))
   ((hash-table? el) (hash-table->syntax el))
   ((bytecode-record? el) `(make-bytecode-record ,(bytecode-record-bytecode el)))
   ((object? el) (object-serialize el))
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

(define (get-both key obj default restrict-module-lookup?)
  (let ((my-result (obj-lookup (object-props obj) key restrict-module-lookup?)))
    (if (eq? my-result *unspecified*)
	(or (and-let* ((parent (object-parent obj)))
	      (get-both key parent default restrict-module-lookup?))
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

(define (desc name child-hash)
  (let ((children 
	 (hash-fold (lambda (key value sofar) (cons value sofar)) '() child-hash)))
    (fold 
     (lambda (child match) 
       (if match match 
	   (if (eq? name (object-name child)) 
	       child 
	       (desc name (object-children child))))) #f children)))

(define (extend parent spec)
  (for-each 
   (lambda (spec-el) 
     (let ((child (if (list? spec-el) 
		      (begin (extend (car spec-el) (cadr spec-el))
			     (car spec-el))
		      spec-el)))       
       (set-object-parent! child parent)
       (hashq-set! (object-children parent) (object-name child) child))) spec))

(define (process-val val me parent args)
  (cond
   ((method? val) 
    (apply ((method-procedure val) me) args))
   ((reference? val) (resolve-reference val))
   ((list? val) (map (lambda (el) (process-val el me parent args)) val))
   (else val)))

(define* (super child #:optional par)
  (and-let* ((parent (or par (object-parent child))))
    (lambda* (#:key (def *unspecified*) #:allow-other-keys . args)	       
      (let* ((parent-clean-args (remove-keyword-args args (list #:def)))
	     (direct-parent (get-containing-object (car parent-clean-args) parent)))
	(let process-super-val ((val (get (car parent-clean-args) direct-parent def)))
	  (cond
	   ((method? val) 
	    (apply ((method-procedure val) 
		    child 
		    (super child (object-parent direct-parent)))
		   (cdr parent-clean-args)))
	   ((reference? val) (resolve-reference val))
	   ((list? val) (map (lambda (el) (process-super-val el)) val))
	   (else val)))))))

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

(define-syntax define-object
  (syntax-rules ()
    ((_ n (key value) ...) 
     (begin (define n 
	      (let ((new-object
		     (object (quote n) (m (key value) ...))))
		(if runtime-prototype-update?
		    (update-existing-objects (quote n) new-object))
		new-object))))))

(define-syntax define-object-public
  (syntax-rules ()
    ((_ n (key value) ...) 
     (begin (define-public n 
	      (let ((new-object
		     (object (quote n) (m (key value) ...))))
		(if runtime-prototype-update?
		    (update-existing-objects (quote n) new-object))
		new-object))))))

(define (update-existing-objects name obj)
  (and-let* (((module-defined? (current-module) name))
	     (previous-obj (module-ref (current-module) name))
	     ((object? previous-obj)))
    (hash-for-each (lambda (uid instance) 
		     (set-object-parent! instance obj))
		   (object-instances previous-obj))))

(define* (object #:optional n properties children-list #:key saved-uid)
  (letrec* 
      ((children (make-hash-table))
       (uid 
	(if (or enable-cross-references? runtime-prototype-update?)
	    (or saved-uid (uid-generator)) 
	    #f))       
       (me (make-object 
	    n #f (if properties properties (make-module)) children 
	    uid (make-weak-value-hash-table))))
    (if (or enable-cross-references? runtime-prototype-update?) (hash-set! uids uid me))
    (if (list? children-list) 
	(map (lambda (child) 
	       (set-object-parent! child me) 
	       (hashq-set! children (object-name child) child))
	     children-list))
    me))

(define (object-reference obj)   
  (if enable-cross-references? 
      (let ((ht (make-weak-value-hash-table)))
	(hash-set! ht 'obj obj)
	(make-reference (object-uid obj) ht))
      (error "Shelf references not enabled (set \"enable-cross-references\" to #t)")))

(define* (object-get-raw obj #:key def . keys) 
  (get (remove-keyword-args keys '(#:def)) obj def))

(define* (object-get obj #:key def . keys) 
  (let ((result (get (remove-keyword-args keys '(#:def))' obj def)))
    (cond 
     ((method? result) ((method-procedure result) obj))
     ((reference? result) (resolve-reference result))
     (else result))))

(define (object-desc obj name)
  (if (eq? (object-name obj) name) obj (desc name (object-children obj))))

(define (object-serialize obj)
  (let ((name (object-name obj))
	(parent (object-parent obj))
	(uid (object-uid obj))
	(props (object-props obj))
	(children (object-children obj)))
   `(,@(if (not name) 
	   (list 'instance (if parent (object-name parent) #f))
	   (list 'object `(quote ,name)))
     ,(module->syntax props)		      
     (list ,@(hash-fold 
	      (lambda (key value sofar) 
		(cons (object-serialize value) sofar)) 
	      '() children))
     ,@(if enable-cross-references? (list #:saved-uid uid) '()))))

(define (object-compile obj mod)
  (make-bytecode-record 
   (compile (object-serialize obj) 
	    #:to 'bytecode 
	    #:env mod)))

(define (object-documentation obj)
  (hash-fold
   (lambda (key val prev) 		      
     (if (method? (variable-ref val))
	 (assq-set! prev key
		    (procedure-documentation 
		     ((method-procedure (variable-ref val)) #f))))) 
   (if (object-parent obj) 
       (object-documentation (object-parent obj)) '()) 
   (module-obarray (object-props obj))))

(define :
 (make-procedure-with-setter 
  (lambda* (obj keys #:key (def *unspecified*) . args)
    (process-val (get keys obj def) obj (object-parent obj) 
		 (remove-keyword-args args '(#:def))))
  (lambda (obj key value) (object-set! key value (object-props obj)))))

(define* (instance par #:optional properties children #:key args saved-uid)
  (let ((child (object #f properties children #:saved-uid saved-uid))
	(parent (cond
		 ((pair? par) (object-desc (car par) (cdr par)))
		 (else par))))
    (set-object-parent! child parent)
    (if args (apply child (cons 'initialize! args))) child))
