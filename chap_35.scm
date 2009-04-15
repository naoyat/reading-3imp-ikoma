;;
;; Chapter 3.5 - Improving Variable Access
;;
(require "./3imp")

; p.64
(define (compile x e next)
  (cond
   [(symbol? x)
	(list 'refer (compile-lookup x e) next)]
   [(pair? x)
	(record-case x
				 [quote (obj)
						(list 'constant obj next)]
				 [lambda (vars body)
				   (list 'close 
						 (compile body (extend* e vars) '(return))
						 next)]
				 [if (test then else)
					 (let ([thenc (compile then e next)]
						   [elsec (compile else e next)])
					   (compile test e (list 'test thenc elsec)))]
				 [set! (var x)
					   (let ([access (compile-lookup var e)])
						 (compile x e (list 'assign access next)))]
				 [call/cc (x)
						  (let ([c (list 'conti
										 (list 'argument
											   (compile x e '(apply))))])
							(if (tail? next)
								c
								(list 'frame next c)))]
				 [else
				  (recur loop ([args (cdr x)]
							   [c (compile (car x) e '(apply))])
						 (if (null? args)
							 (if (tail? next)
								 c
								 (list 'frame next c))
							 (loop (cdr args)
								   (compile (car args)
											s
											(list 'argument c)))))])]
   [else
	(list 'constant x next)]))

; p.65
(define (extend* e r) (cons r e))

(define (compile-lookup var e)
  (recur nxtlib ([e e] [rib 0])
		 (recur nxtelt ([vars (car e)] [elt 0])
				(cond
				 [(null? vars) (nxtrib (cdr e) (+ rib 1))]
				 [(eq? (car vars) var) (cons rib elt)]
				 [else (nxtelt (cdr vars) (+ elt 1))]))))

; p.66
(define (VM a x e r s)
  (record-case x
			   [halt () a]
			   [refer (var x)
					  (VM (car (lookup var e)) x e r s)]
			   [constant (obj x)
						 (VM obj x e r s)]
			   [close (body x)
					  (VM (closure body e) x e r s)]
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
					  (record (body e) a
							  (VM a body (extend* e r) '() s))]
			   [return ()
					   (record (x e r s) s
							   (VM a x e r s))]))

(define (closure body e) (list body e))

; p.67
(define (continuation s)
  (closure (list 'nuate s '(0 . 0)) '()))

(define (lookup access e)
  (recur nxtrib ([e e] [rib (car access)])
		 (if (= rib 0)
			 (recur nxtelt ([r (car e)] [elt (cdr access)])
					(if (= elt 0)
						r
						(nxtelt (cdr r) (- elt 1))))
			 (nxtrib (cdr e) (- rib 1)))))

