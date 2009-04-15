(require "./3imp")

(define (meta exp)
  (exec exp '()))

(define (exec exp env)
  (cond [(symbol? exp) (car (lookup exp env))]
	 [(pair? exp)
	  (record-case exp
				   [quote (obj) obj]
				   [lambda (vars body)
					 (lambda (vals)
					   (exec body (ore-extend env vars vals)))]
				   [if (test then else)
					   (if (exec test env)
						   (exec then env)
						   (exec else env))]
				   [set! (var val)
						 (set-car! (lookup var env) (exec val env))]
				   [call/cc (exp)
							(call/cc
							 (lambda (k)
							   ((exec exp env)
								(list (lambda (args) (k (car args)))))))]
;				   [call/cc (exp) (call/cc (exec exp env))]
				   [else
					((exec (car exp) env)
					 (map (lambda (x) (exec x env)) (cdr exp)))])]
		[else exp]))

(define (lookup var e)
  (recur nxtrib ([e e])
	(recur nxtelt ([vars (caar e)] [vals (cdar e)])
	  (cond [(null? vars) (nxtrib (cdr e))]
			[(eq? (car vars) var) vals]
			[else (nxtelt (cdr vars) (cdr vals))]))))

(define (ore-extend env vars vals) ;;; extend
  (cons (cons vars vals) env))

;((lambda (x) (+ x 1)) 7)
;(lambda (x y)
;  (set! x 10)
;  (set! y (lambda (z) (* z z)))
;  (y x))

(use gauche.test)

(test-start "Chapter 2")

(test-section "page 40")
(test* "symbol?" 2 (exec 'b '(((a b c) . (1 2 3)))))
(test* "quote" 'hello (meta '(quote hello)))
(test* "lambda" 3 (meta '((lambda (x) x) 3)))
(test* "if" 5 (meta '(if #t 5 0)))
(test* "set!" 7 (meta '((lambda (t) ((lambda (x) t) (set! t 7))) 0)))
(test* "call/cc" 11 (meta '(((call/cc (lambda (c) c)) (lambda (x) x)) 11)))
(test* "pair:else?" 13 (meta '((lambda (f x) (f x)) (lambda (x) x) 13)))
(test* "else?" 17 (meta 17))
(test-end)
