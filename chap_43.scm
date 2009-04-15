;;
;; Chapter 4.3 - Stack Allocating the Static Chain
;;
(require "./3imp")

(require "./chap_35") ;closure, extend*,
(require "./chap_41") ;compile-lookup(3)
(require "./chap_42") ;restore-stack

; p.86
(define (continuation s)
  (closure
   (list 'refer 0 0 (list 'nuate (save-stack s) '(return 0)))
   '()))

; p.87
(define (compile x e next)
  (cond
   [(symbol? x)
	(compile-lookup x e
					(lambda (n m)
					  (list 'refer n m next)))]
   [(pair? x)
	(record-case x
				 [quote (obj)
						(list 'constant obj next)]
				 [lambda (vars body)
				   (list 'close
						 (compile body
								  (extend* e vars)
								  (list 'return (+ (length vars) 1)))
						 next)]
				 [if (test then else)
					 (let ([thenc (compile then e next)]
						   [elsec (compile else e next)])
					   (compile test e (list 'test thenc elsec)))]
				 [call/cc (x)
						  (list 'frame
								next
								(list 'conti
									  (list 'argument
											(compile x e '(apply)))))]
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

(define (VM a x e s)
  (record-case x
			   [halt () a]
			   [refer (n m x)
						 (VM (index (find-link n e) m) x e s)]
			   [constant (obj x)
						 (VM obj x e s)]
			   [close (body x)
					  (VM (closure body e) x e s)]
			   [test (then else)
					 (VM a (if a then else) e s)]
			   [conti (x)
					  (VM (continuation s) x e s)]
			   [nuate (stack x)
					  (VM a x e (restore-stack stack))]
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

