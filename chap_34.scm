(require "./3imp")

; p.56
(define (compile x next)
  (cond [(symbol? x)
		 (list 'refer x next)]
		[(pair? x)
		 (record-case x
					  [quote (obj)
							 (list 'constant obj next)]
					  [lambda (vars body)
						(list 'close vars (compile body '(return)) next)]
					  [if (test then else)
						  (let ([thenc (compile then next)]
								[elsec (compile else next)])
							(compile test (list 'test thenc elsec)))]
					  [set! (var x)
							(compile x (list 'assign var next))]
					  [call/cc (x)
							   (let ([c (list 'conti
											  (list 'argument
													(compile x '(apply))))])
								 (if (tail? next)
									 c
									 (list 'frame next c)))]
					  [else
					   (recur loop ([args (cdr x)]
									[c (compile (car x) '(apply))])
							  (if (null? args)
								  (if (tail? next)
									  c
									  (list 'frame next c))
								  (loop (cdr args)
										(compile (car args)
												 (list 'argument c)))))])]
		[else
		 (list 'constant x next)]))

; p.59
(define (tail? next)
  (eq? (car next) 'return))

; p.60
(define (VM a x e r s)
  (record-case x
			   [halt () a]
			   [refer (var x)
					  (VM (car (lookup var e)) x e r s)]
			   [constant (obj x)
						 (VM obj x e r s)]
			   [close (vars body x)
					  (VM (closure body e vars) x e r s)]
			   [test (then else)
					 (VM a (if a then else) e r s)]
			   [assign (var x)
					   (set-car! (lookup var e) a)
					   (VM a x e r s)]
			   [conti (x)
					  (VM (continuation s) x e r s)]
			   [nuate (s var)
					  (VM (car (lookup var e)) '(return) e r s)]
			   [frame (ret x)
					  (VM a x e '() (call-frame ret e r s))]
			   [argument (x)
						 (VM a x e (cons a r) s)]
			   [apply ()
					  (record (body e vars) a
							  (VM a body (extend* e vars r) '() s))]
			   [return ()
					   (record (x e r s) s
							   (VM a x e r s))]))

; p.61
(define (lookup var e)
  (recur nxtrib ([e e])
		 (recur nxtelt ([vars (caar e)] [vals (cdar e)])
				(cond [(null? vars) (nxtrib (cdr e))]
					  [(eq? (car vars) var) vals]
					  [else (nxtelt (cdr vars) (cdr vals))]))))

(define (closure body env vars)
  (list body e vars))

(define (continuation s)
  (closure (list 'nuate s 'v) '() '(v)))

(define (call-frame x e r s)
  (list x e r s))

; p.62
(define (extend* e vars vals)
  (cons (cons vars vals) e))

(define (evaluate x)
  (VM '() (compile x '(halt)) '() '() '()))

(define (eval* x)
  (let1 compiled-code (compile x '(halt))
	(print compiled-code)
	(VM '() compiled-code '() '() '())
	))
