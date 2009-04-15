;;; 2009/3/6
;;;
;;; 4.4 Display Closures ディスプレイ・クロージャ
;;;
(require "./3imp")

;;(require "./chap_41") ;stack, push, index ..
(define stack (make-vector 1000))

(define (push x s)
  (vector-set! stack s x)
  (+ s 1))

(define (index s i) ; = stack[s-i-1]
  (vector-ref stack (- s i 1)))

;;(require "./chap_42") ;save-stack ;restore-stack

(define (save-stack s) ;; v <== stack[0..s-1]; vを返すよ
  (let1 v (make-vector s)
	(dotimes (i s) (vector-set! v i (vector-ref stack i)))
	v))
(define (restore-stack v) ;; stack <== v; 長さを返すよ
  (let1 s (vector-length v)
	(dotimes (i s) (vector-set! stack i (vector-ref v i)))
	s))

;(require "./chap_43") ; continuation

(use srfi-1)
(define (head-of-stack)
  (let1 undef (if #f #f)
	(filter (lambda (x) (not (eq? undef x)))
			(take (vector->list stack) 20) )))

;; (body e) => vec <body n s>
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

;;
;; 4.4.1 Displays. ディスプレイ
;;

;;
;; 4.4.2 Creating Display Closures. ディスプレイ・クロージャを作る
;;

;;
;; 4.4.3 Finding Free Variables. 自由変数を探す
;;

;; p.92

(define (find-free x b)
  (cond [(symbol? x)
		 (if (set-member? x b) '() (list x))]
		[(pair? x)
		 (record-case x
					  [quote (obj) '()]
					  [lambda (vars body)
						(find-free body (set-union vars b))]
					  [if (test then else)
						  (set-union (find-free test b)
									 (set-union (find-free then b)
												(find-free else b)))]
					  [call/cc (exp) (find-free exp b)]
					  [else (let next ([x x])
							  (if (null? x)
								  '()
								  (set-union (find-free (car x) b)
											 (next (cdr x)))))])]
		[else '()]))

;; p.93

(define (set-member? x s)
;  (cond [(null? s) '()]
;		[(eq? x (car s)) 't]
  (cond [(null? s) #f]
		[(eq? x (car s)) #t]
		[else (set-member? x (cdr s))]))

(define (set-cons x s)
  (if (set-member? x s)
	  s
	  (cons x s)))

(define (set-union s1 s2)
  (if (null? s1)
	  s2
	  (set-union (cdr s1) (set-cons (car s1) s2))))

(define (set-minus s1 s2)
  (if (null? s1)
	  '()
	  (if (set-member? (car s1) s2)
		  (set-minus (cdr s1) s2)
		  (cons (car s1) (set-minus (cdr s1) s2)))))

(define (set-intersect s1 s2)
  (if (null? s1)
	  '()
	  (if (set-member? (car s1) s2)
		  (cons (car s1) (set-intersect (cdr s1) s2))
		  (set-intersect (cdr s1) s2))))

;;
;; 4.4.4 Translation 翻訳
;;

;; p.94

(define (compile x e next)
  (cond
   [(symbol? x)
	(compile-refer x e next)]
   [(pair? x)
	(record-case x
				 [quote (obj) (list 'constant obj next)]
				 [lambda (vars body)
				   (let1 free (find-free body vars)
					 (collect-free free e
								   (list 'close
										 (length free)
										 (compile body
												  (cons vars free)
												  (list 'return
														(length vars)))
										 next)))]
				 [if (test then else)
					 (let ([thenc (compile then e next)]
						   [elsec (compile else e next)])
					   (compile test e (list 'test thenc elsec)))]
				 [call/cc (x)
						  (list 'frame
								(list 'conti
									  0 ;;;
									  (list 'argument
											(compile x e '(apply))))
								next)]
;						  (list 'frame
;								next
;								(list 'conti
;									  (list 'argument
;											(compile x e '(apply)))))]
				 [else
				  (let loop ([args (cdr x)]
							 [c (compile (car x) e '(apply))])
					(if (null? args)
						(list 'frame next c)
						(loop (cdr args)
							  (compile (car args)
									   e
									   (list 'argument c)))))])]
   [else (list 'constant x next)]))

;; p.95

(define (collect-free vars e next)
  (if (null? vars)
	  next
	  (collect-free (cdr vars) e
					(compile-refer (car vars) e
								   (list 'argument next)))))

(define (compile-refer x e next)
  (compile-lookup x e
				  (lambda (n) (list 'refer-local n next))
				  (lambda (n) (list 'refer-free n next))))

(define (compile-lookup x e return-local return-free)
  (let next-local ([locals (car e)] [n 0])
	(if (null? locals)
		(let next-free ([free (cdr e)] [n 0])
		  (if (eq? (car free) x)
			  (return-free n)
			  (next-free (cdr free) (+ n 1))))
		(if (eq? (car locals) x)
			(return-local n)
			(next-local (cdr locals) (+ n 1))))))

;; p.96

;;
;; 4.4.5 Evaluation 評価
;;

;; p.97
(define (VM a x f c s)
  (format #t "% VM\n")
  (format #t "  ACCUM: ~a\n" a)
  (format #t "  NEXT: ~a\n" x)
  (format #t "  CURR-CALL-FRAME: ~a\n" f) ;; [eから改名] (環境は現在のフレームと現在のクロージャに分けて保存されるようになったので)
  (format #t "  CURR-CLOSURE: ~a\n" c) ;; [NEW]クロージャ外の自由変数を参照するときに使う
  (format #t "  STACK: ~d in ~a\n\n" s (head-of-stack))

  (record-case x
			   ;; halt
			   [halt () a] ;; aを返す
			   ;; refer-local: ◎カレントコールフレーム f に入っている n 番目の引数値をロードし、アキュムレータへ
			   [refer-local (n x)
							(VM (index f n) x f c s)] ;; a = index(f,n); x = x
			   ;; refer-free: ◎カレントクロージャ c に入っている n 番目の自由変数値をロードし、アキュムレータへ
			   [refer-free (n x)
						   (VM (index-closure c n) x f c s)] ;; a = index-closure(c,n), x = x
			   ;; constant
			   [constant (obj x) ;
						 (VM obj x f c s)] ;; a = obj, x = x
			   ;; close: ◎ ここでクロージャは、関数本体と、関数の自由変数の値の入ったベクタ。
			   ;;           この命令は、body, スタックから上nアイテムを取って来て、このベクタを作り、アキュムレータに置く。
			   [close (n body x)
					  (VM (closure body n s) x f c (- s n))] ;; a = closure(body,n,s), x = x, s -= n
			   ;; test
			   [test (then else)
					 (VM a (if a then else) f c s)] ;; x = a ? then : else
;			   [conti (x)
;					  (VM (continuation s) x f c s)]
			   ;; conti
			   [conti (n x)
					  (VM (continuation s n) x f c s)] ;; a = continuation(s,n); x = x
			   ;; // 保存しておいたスタックを戻す
			   [nuate (stack x)
					  (VM a x f c (restore-stack stack))] ;; x = x; s = restore-stack(stack)
			   ;; frame: ◎カレントクロージャ c とカレントフレームポインタ f を保存。（前まではフレームポインタだけ保存してた）
			   ;;          // スタックに c, f, ret を積む。s+=3 か
			   [frame (ret x)
					  (VM a x f c (push ret (push f (push c s))))] ;; x = x; s << [c,f,ret]
			   ;; argument
			   [argument (x)
						 (VM a x f c (push a s))] ;; x = x; s << a
			   ;; apply
			   [apply ()
					  (VM a (closure-body a) s a s)] ;; x = closure-body(a); f = s; c = a
			   ;; return: ◎クロージャとフレームポインタをリストア。（前まではフレームポインタ） [ 0>next 1>f 2>c ]
			   [return (n)
					   (let1 s (- s n)
						 (VM a (index s 0) (index s 1) (index s 2) (- s 3)))]))

(define (closure body n s)
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
  (vector-ref c 0))
(define (index-closure c n)
  (vector-ref c (+ n 1)))

(define (evaluate x)
  (VM
   ;; a <expr>
   '() ; <y>空リスト
   ;; x <opc>
   (compile x '() '(halt)) ;; <y>コンパイルされた式
   ;; f
   0 ;; <y>現在のフレームへのポインタ
   ;; c
   '() ;; <y>空リスト(正しいプログラムは自由変数を含まないはずなので、この空リストの中身は参照されないはず)
   ;; s
   0))
