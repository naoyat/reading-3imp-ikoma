;;;
;;; 4.6 Tail Calls 末尾呼出し
;;;
(require "./3imp")

(require "./chap_41") ;; index-set!
(require "./chap_45") ;; find-free

;; p.110
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
						(let ([free (find-free body vars)] ;, f
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
							   (let1 c (list 'conti
											 (if (tail? next) (cadr next) 0);;;;;
											 (list 'argument
												   (compile x e s
															(if (tail? next)
																(list 'shift
																	  1
																	  (cadr next)
																	  '(apply))
																'(apply)))))
								 (if (tail? next)
									 c
;									 (list 'frame next c)))]
									 (list 'frame c next)))]
					  [else
					   (let loop (;[func (car x)]
								  [args (cdr x)]
								  [c (compile (car x) e s
											  (if (tail? next)
												  (list 'shift (length (cdr x)) (cadr next) '(apply))
												  '(apply)))])
;												  (list 'shift (length args) (cadr next) (list 'apply (length args)))
;												  (list 'apply (length args))))])
						 (if (null? args)
							 (if (tail? next)
								 c
										;									  (list 'frame next c))
								 (list 'frame c next))
							 (loop ;(car args)
								   (cdr args)
								   (compile (car args) e s (list 'argument c)))))])]
		[else (list 'constant x next)]))


;; p.111

;;
;; 4.6.3 Evaluation.
;;
(define (shift-args n m s)
  (let next-arg ((i (- n 1)))
	(unless (< i 0)
	  (index-set! s (+ i m) (index s i))
	  (next-arg (- i 1))))
  (- s m))

;; p.112
(define (VM a x f c s)
  (format #t "% VM\n")
  (format #t "  ACCUM: ~a\n" a)
  (format #t "  NEXT: ~a\n" x)
  (format #t "  CURR-CALL-FRAME: ~a\n" f) ;; [eから改名] (環境は現在のフレームと現在のクロージャに分けて保存されるようになったので)
  (format #t "  CURR-CLOSURE: ~a\n" c) ;; [NEW]クロージャ外の自由変数を参照するときに使う
  (format #t "  STACK: ~d in ~a\n\n" s (head-of-stack))

  (record-case x
			   [halt () a]
			   [refer-local (n x)
							(VM (index f n) x f c s)]
			   [refer-free (n x)
						   (VM (index-closure c n) x f c s)] ; index-closure c n = c[n+1]
			   [indirect (x)
						 (VM (unbox a) x f c s)]
			   [constant (obj x)
						 (VM obj x f c s)]
			   [close (n body x)
					  (VM (closure body n s) x f c (- s n))]
			   [box (n x)
					(index-set! s n (box (index s n)))]
			   [test (then else)
					 (VM a (if a then else) f c s)]
			   [assign-local (n x)
							 (set-box! (index f n) a)
							 (VM a x f c s)]
			   [assign-free (n x)
							(set-box! (index-closure c n) a); c[n+1].[0] = a
							(VM a x f c s)]
;			   [conti (x)
;					  (VM (continuation s) x f c s)]
			   [conti (n x)
					  (VM (continuation s n) x f c s)]
			   [nuate (stack x)
					  (VM a x f c (restore-stack stack))]
;			   [frame (ret x)
;					  (VM a x f c (push ret (push f (push c s))))]
			   [frame (x ret)
					  (VM a x f c (push ret (push f (push c s))))]
			   [argument (x)
						 (VM a x f c (push a s))]
			   [shift (n m x)
					  (VM a x f c (shift-args n m s))]
			   [apply ()
					  (VM a (closure-body a) s a s)] ;; closure-body c = c[0]
			   [return (n)
					   (let1 s (- s n)
						 (VM a (index s 0) (index s 1) (index s 2) (- s 3)))]
			   ))

(define (tail? x)
  (eq? 'return (car x)))

(define (box x)
  (list x))
(define (unbox x)
  (car x))
(define (set-box! b x)
  (set-car! b x))

;(define (continuation s)
;  (closure
;   (list 'refer 0 0 (list 'nuate (save-stack s) '(return 0)))
;   '()))
;(define (continuation s n)
;  (closure
;   (list 'refer-local 0 0 (list 'nuate (save-stack s) '(return 0)))
;   '()))

(define (continuation s n) ;; ３引数のclosureを使う版continuationの定義がない ... biwaschemeから
  (closure
;   (list 'refer 0 0 (list 'nuate (save-stack s) '(return 0)))
   ;; body
   (list 'refer-local; 0 0
		 0;n?
		 (list 'nuate (save-stack s) (list 'return n))) ; next?
   ;; n - number of frees
   0
   ;; s:x
   '()
   ))


