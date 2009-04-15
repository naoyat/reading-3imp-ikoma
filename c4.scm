;;;
;;; 4.6 Tail Calls 末尾呼出し
;;;
(require "./3imp")
(require "./debug")
(use srfi-1)

;;
;; Chapter 4.1 - Stack-Based Implementation of Block-Structured Languages
;;

; p.75
(define stack (make-vector 1000))

(define (push x s)
  (dbg :trace "(push x:~a s:~d)" x s)
  (vector-set! stack s x)
  (+ s 1))

(define (index s i) ;**
  (dbg :trace "(index s:~d i:~d) => stack[~d]" s i (- s i 1))
;  (vector-ref stack (- (- s i) 1)))
  (vector-ref stack (- s i 1)))

(define (index-set! s i v) ;**
  (dbg :trace "(index-set! s:~d i:~d v:~a) => stack[~d] := ~a" s i v  (- s i 1) v)
;  (vector-set! stack (- (- s i) 1) v)
  (vector-set! stack (- s i 1) v))


(define (save-stack s) ;; v <== stack[0..s-1]; vを返すよ
  (dbg :trace "(save-stack s:~a)" s)
  (let1 v (make-vector s)
	(dotimes (i s) (vector-set! v i (vector-ref stack i)))
	v))
(define (restore-stack v) ;; stack <== v; 長さを返すよ
  (dbg :trace "(restore-stack v:~a)" v)
  (let1 s (vector-length v)
	(dotimes (i s) (vector-set! stack i (vector-ref v i)))
	s))

(define (head-of-stack)
  (let1 undef (if #f #f)
	(filter (lambda (x) (not (eq? undef x)))
			(take (vector->list stack) 20) )))

;;;
;;; 4.4 Display Closures ディスプレイ・クロージャ
;;;

;;
;; 4.4.1 Displays. ディスプレイ
;;

;;
;; 4.4.2 Creating Display Closures. ディスプレイ・クロージャを作る
;;

;;
;; 4.4.3 Finding Free Variables. 自由変数を探す
;;

;; p.93

(define (set-member? x s)
  (dbg :trace "(set-member? x:~a s:~a)" x s)
;  (cond [(null? s) '()]
;		[(eq? x (car s)) 't]
  (cond [(null? s) #f]
		[(eq? x (car s)) #t]
		[else (set-member? x (cdr s))]))

(define (set-cons x s)
  (dbg :trace "(set-cons x:~a s:~a)" x s)
  (if (set-member? x s)
	  s
	  (cons x s)))

(define (set-union s1 s2)
  (dbg :trace "(set-union s1:~a s2:~a)" s1 s2)
  (if (null? s1)
	  s2
	  (set-union (cdr s1) (set-cons (car s1) s2))))

(define (set-minus s1 s2)
  (dbg :trace "(set-minus s1:~a s2:~a)" s1 s2)
  (if (null? s1)
	  '()
	  (if (set-member? (car s1) s2)
		  (set-minus (cdr s1) s2)
		  (cons (car s1) (set-minus (cdr s1) s2)))))

(define (set-intersect s1 s2)
  (dbg :trace "(set-intersect s1:~a s2:~a)" s1 s2)
  (if (null? s1)
	  '()
	  (if (set-member? (car s1) s2)
		  (cons (car s1) (set-intersect (cdr s1) s2))
		  (set-intersect (cdr s1) s2))))

;;
;; 4.4.4 Translation 翻訳
;;

;; p.95

(define (collect-free vars e next)
  (dbg :trace "(collect-free vars:~a e:~a next:~a)" vars e next)
  (if (null? vars)
	  next
	  (collect-free (cdr vars) e
					(compile-refer (car vars) e
								   (list 'argument next)))))

(define (compile-refer x e next)
  (dbg :trace "(compile-refer x:~a e:~a next:~a)" x e next)
  (compile-lookup x e
				  (lambda (n) (list 'refer-local n next))
				  (lambda (n) (list 'refer-free n next))))

(define (compile-lookup x e return-local return-free)
  (dbg :trace "(compile-lookup x:~a e:~a return-local:~a return-free:~a)" x e return-local return-free)
  (unless (null? e)
	(let next-local ([locals (car e)] [n 0])
	  (dbg :compile "COMPILE> (next-local locals:~a, n:~d)" locals n)
	  (if (null? locals)
		  (let next-free ([free (cdr e)] [n 0])
			(dbg :compile "COMPILE> (next-free free:~a, n:~d)" free n)
			(if (eq? (car free) x)
				(return-free n)
				(next-free (cdr free) (+ n 1))))
		  (if (eq? (car locals) x)
			  (return-local n)
			  (next-local (cdr locals) (+ n 1)))))
	))

;;
;; 4.4.5 Evaluation 評価
;;

(define (closure body n s)
  (dbg :trace "(closure body:~a n:~a s:~a)" body n s)
  (let1 v (make-vector (+ n 1))
	(vector-set! v 0 body)
;	(let f ([i 0])
;	  (unless (= i n)
;		(vector-set! v (+ i 1) (index s i))
;		(f (+ i 1))))
	(dotimes (i n) (vector-set! v (+ i 1) (index s i)))
	v))

;; p.98
(define (closure-body c)
  (dbg :trace "(closure-body c:~a)" c)
  (vector-ref c 0))
(define (index-closure c n)
  (dbg :trace "(index-closure c:~a n:~a)" c n)
  (vector-ref c (+ n 1)))

;;;;;;;;;;;;;;;;;;;;;;;45
;;;
;;; 4.5 Supporting Assignments 代入のサポート
;;;

;;
;; 4.5.1 Translation. 翻訳
;;
(define (find-sets x v)
  (dbg :trace "(find-sets x:~a v:~a)" x v)
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
  (dbg :trace "(make-boxes sets:~a vars:~a next:~a)" sets vars next)
  (let f ([vars vars] [n 0])
	(if (null? vars)
		next
		(if (set-member? (car vars) sets)
			(list 'box n (f (cdr vars) (+ n 1)))
			(f (cdr vars) (+ n 1))))))

;; p.104
(define (find-free x b)
  (dbg :trace "(find-free x:~a b:~a)" x b)
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

;;
;; 4.6
;;
;; p.110
(define (compile x e s next)
  (dbg :compile "% COMPILE")
  (dbg :compile "  X: ~a" x)
  (dbg :compile "  ENV: ~a" e)
  (dbg :compile "  S: ~a" s)
  (dbg :compile "  NEXT: ~a\n" next)

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
							(dbg :compile "COMPILE> set! var:~a x:~a" var x)
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

;;
;; 4.6.3 Evaluation.
;;
(define (shift-args n m s) ;; STACK[s+(n-1..1)+m-1] = STACK[s-(n-1..1)-1]
  (dbg :trace "(shift-args n:~d m:~d s:~d)" n m s)
  (let next-arg ((i (- n 1)))
	(unless (< i 0)
	  (index-set! s (+ i m) (index s i))
	  (next-arg (- i 1))))
;	(index-set! s (+ i m) (index s i))
;	(unless (< i 0)
;	  (next-arg (- i 1))))
  (- s m))

;; p.112
(define (VM a x f c s)
  (dbg :vm "% VM")
  (dbg :vm "  ACCUM: ~a" a)
  (dbg :vm "  NEXT: ~a" x)
  (dbg :vm "  CURR-CALL-FRAME: ~a" f) ;; [eから改名] (環境は現在のフレームと現在のクロージャに分けて保存されるようになったので)
  (dbg :vm "  CURR-CLOSURE: ~a" c) ;; [NEW]クロージャ外の自由変数を参照するときに使う
  (dbg :vm "  STACK: ~d in ~a" s (head-of-stack))
  (dbg :vm "  INST: ~a\n" (car x))

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
					(index-set! s n (box (index s n)))
					(VM a x f c s)]
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
			   [_ args
				  (dbg :vm "?? ~a ~a" x args)]
			   ))

(define (tail? x)
  (dbg :trace "(tail? x:~a)" x)
  (eq? 'return (car x)))

(define (box x)
  (dbg :trace "(box x:~a)" x)
  (list x))
(define (unbox x)
  (dbg :trace "(unbox x:~a)" x)
  (car x))
(define (set-box! b x)
  (dbg :trace "(set-box! b:~a x:~a)" b x)
  (set-car! b x))

(define (continuation s n) ;; ３引数のclosureを使う版continuationの定義がない ... biwaschemeから
  (dbg :trace "(continuation s:~a n:~a)" s n)
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

(define (evaluate x . args)
  (let-optionals* args ((env '()))
	(dbg :evaluate "% EVALUATE")
	(dbg :evaluate "  X: ~a" x)
	(dbg :evaluate "  ENV: ~a\n" env)

	(VM '()
		(compile x env '() '(halt))
		0
		'()
		0)
	))
