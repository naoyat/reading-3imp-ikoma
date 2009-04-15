;;
;; Chapter 4.1 - Stack-Based Implementation of Block-Structured Languages
;;
(require "./3imp")

; p.74
(define (functional body e) (list body e))

; p.75
(define stack (make-vector 1000))

(define (push x s)
  (vector-set! stack s x)
  (+ s 1))

(define (index s i)
  (vector-ref stack (- (- s i) 1)))

(define (index-set! s i v)
  (vector-set! stack (- (- s i) 1) v))

; p.76
(define (compile x e next)
  (cond [(symbol? x)
		 (compile-lookup x e (lambda (n m) (list 'refer n m next)))]
		[(pair? x)
		 (record-case x
					  [quote (obj) (list 'constant obj next)]
					  [lambda (vars body) (list 'close
												(compile body
														 (extend* e vars)
														 (list 'return (+ (length vars) 1)))
												next)]
					  [if (test then else)
						  (let ([thenc (compile then e next)]
								[elsec (compile else e next)])
							(compile test e (list 'test thenc elsec)))]
					  [set! (var x)
							(compile-lookup var e
											(lambda (n m)
											  (compile x e (list 'assign n m next))))]
					  [else
					   (recur loop ([args (cdr x)]
									[c (compile (car x) e '(apply))])
							  (if (null? args)
								  (list 'frame next c)
								  (loop (cdr args)
										(compile (car args)
												 e
												 (list 'argument c)))))])]
		[else
		 (list 'constant x next)]))

; p.77
(define (compile-lookup var e return)
  (recur nxtrib ([e e] [rib 0])
		 (recur nxtelt ([vars (car e)] [elt 0])
				(cond
				 [(null? vars) (nxtrib (cdr e) (+ rib 1))]
				 [(eq? (car vars) var) (return rib elt)]
				 [else (nxtelt (cdr vars) (+ elt 1))]))))

; p.78
(define (extend* e r) (cons r e))

; p.79
(define (VM a x e s)
  (record-case x
			   [halt () a]
			   [refer (n m x)
					  (VM (index (find-link n e) m) x e s)]
			   [constant (obj x)
						 (VM obj x e s)]
			   [close (body x)
					  (VM (functional body e) x e s)]
			   [test (then else)
					 (VM a (if a then else) e s)]
			   [assign (n m x)
					   (index-set! (find-link n e) m a)
					   (VM a x e s)]
			   [frame (ret x)
					  (VM a x e (push ret (push e s)))]
			   [argument (x)
						 (VM a x e (push a s))]
			   [apply ()
					  (record (body link) a
							  (VM a body s (push link s)))]
			   [return (n)
					   (let ([s (- s n)])
						 (VM a (index s 0) (index s 1) (- s 2)))]))

; p.80
(define (find-link n e)
  (if (= n 0)
	  e
	  (find-link (- n 1) (index e -1))))

(define (evaluate x)
  (VM '() (compile x '() '(halt)) 0 0))
