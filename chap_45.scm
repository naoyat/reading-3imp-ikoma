;;;
;;; 4.5 Supporting Assignments 代入のサポート
;;;
(require "./3imp")
(require "./chap_44") ; set-member?

#;(define (continuation s)
  (closure
   (list 'refer 0 0 (list 'nuate (save-stack s) '(return 0)))
   '()))
;(define (closure body e) (list body e))

;; p.98

;; p.99

;; p.100

;; p.101

;;
;; 4.5.1 Translation. 翻訳
;;
(define (find-sets x v)
  (cond [(symbol? x) '()]
		[(pair? x) (record-case x
								[quote (obj) '()]
								[lambda (vars body)
								  (find-sets body (set-minus v vars))]
								[if (test then else)
									(set-union (find-sets test v)
											   (set-union (find-sets then v)
														  (find-sets else v)))]
								[set! (var x)
;									  (set-union (if (set-member? var v) (list var) '())
;												 (find-sets x v))]
									  (if (set-member? var v)
										  (set-cons var (find-sets x v))
										  (find-sets x v))]
								[call/cc (exp) (find-sets exp v)]
								[else (let next ([x x])
										(if (null? x)
											'()
											(set-union (find-sets (car x) v)
													   (next (cdr x)))))])]
		[else '()]))

;; p.102
(define (make-boxes sets vars next)
  (let f ([vars vars] [n 0])
	(if (null? vars)
		next
		(if (set-member? (car vars) sets)
			(list 'box n (f (cdr vars) (+ n 1)))
			(f (cdr vars) (+ n 1))))))

;; p.103
(define (compile x e s next)
  (cond [(symbol? x)
		 (compile-refer x e
						(if (set-member? x s)
							(list 'indirect next)
							next))]
		[(pair? x)
		 (record-case x
					  [quote (obj) (list 'constant obj next)]
					  [lambda (vars body)
						(let ([free (find-free body vars)]
							  [sets (find-sets body vars)])
						  (collect-free free e
										(list 'close
											  (length free)
											  (make-boxes sets vars
														  (compile body
																   (cons vars free)
																   (set-union
																	sets
																	(set-intersect s free))
																   (list 'return (length vars))))
											  next)))]
					  [if (test then else)
						  (let ([thenc (compile then e s next)]
								[elsec (compile else e s next)])
							(compile test e s (list 'test thenc elsec)))]
					  [set! (var x)
							(compile-lookup var e
											(lambda (n)
											  (compile x e s (list 'assign-local n next)))
											(lambda (n)
											  (compile x e s (list 'assign-free n next))))]
					  [call/cc (x)
							   (list 'frame
									 next
									 (list 'conti
										   (list 'argument
												 (compile x e s '(apply)))))]
					  [else
					   (let loop ([args (cdr x)]
								  [c (compile (car x) e s '(apply))])
						 (if (null? args)
							 (list 'frame next c)
							 (loop (cdr args)
								   (compile (car args)
											e
											s
											(list 'argument c)))))])]
		[else (list 'constant x next)]))

;; p.104
(define (find-free x b)
  (cond [(symbol? x) (if (set-member? x b) '() (list x))]
		[(pair? x)
		 (record-case x
					  [quote (obj) '()]
					  [lambda (vars body)
						(find-free body (set-union vars b))]
					  [if (test then else)
						  (set-union (find-free test b)
									 (set-union (find-free then b)
												(find-free else b)))]
					  [set! (var exp)
;							(set-union (if (set-member? var b) '() (list var))
;									   (find-free exp b))]
							(if (set-member? var b)
								(find-free exp b)
								(set-cons var (find-free exp b)))]
					  [call/cc (exp)
							   (find-free exp b)]
					  [else
					   (let next ([x x])
						 (if (null? x)
							 '()
							 (set-union (find-free (car x) b)
										(next (cdr x)))))])]
		[else '()]))

;; p.105

;;
;; 4.5.2 Evaluation. 評価
;;
(define (VM a x f c s)
  (record-case x
			   [halt () a]
			   [refer-local (n x)
							(VM (index f n) x f c s)]
			   [refer-free (n x)
						   (VM (index-closure c n) x f c s)]
			   [indirect (x)
						 (VM (unbox a) x f c s)]
			   [constant (obj x)
						 (VM obj x f c s)]
			   [close (n body x)
					  (VM (closure body n s) x f c (- s n))]
			   [box (n x)
					(index-set! s n (box (index s n)))
					(VM a x f c s)]
			   [test (then else)
					 (VM a (if a then else) f c s)]
			   [assign-local (n x)
							 (set-box! (index-closure c n) a)
							 (VM a x f c s)]
			   [assign-free (n x)
							(set-box! (index-closure c n) a)
							(VM a x f c s)]
			   [conti (x)
					  (VM (continuation s) x f c s)]
			   [nuate (stack x)
					  (VM a x f c (restore-stack stack))]
			   [frame (ret x)
					  (VM a x f c (push ret (push f (push c s))))]
			   [argument (x)
						 (VM a x f c (push a s))]
			   [apply ()
					  (VM a (closure-body a) s a s)]
			   [return (n)
					   (let1 s (- s n)
						 (VM a (index s 0) (index s 1) (index s 2) (- s 3)))]
			   ))

;; p.106
(define (evaluate x)
  (VM '() (compile x '() '() '(halt)) 0 '() 0))

